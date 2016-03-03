#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <linux/joystick.h>

#include "config.h"
#include "midiprotocol.h"

#define CHAR_BUF_SIZE 1024

static int cs;

%%{


     machine conf_parser;

     action capture_param {
//	  printf("para: %s\n", p);
	  map->param = (strtoul(p, NULL, 16) & 0x7F);
     }
     
     action capture_chan {
//	  printf("cha %s\n", p);
	  map->chan = (strtoul(p, NULL, 16) & 0xF);
     }
     
     action capture_cmd {
//	  printf("cmd %s\n", p);
	  map->cmd = (strtoul(p, NULL, 16) & 0xF0);
	  /*
	  map->cmd = (strtoul(p, NULL, 16) & 0xFF);
	  if (!((map->cmd & 0xF0) == 0xF0))
	       map->cmd = map->cmd & 0xF0;
	  */
     }

     action capture_id_type {
	  type =  tolower(*p);

     }
     action capture_id_no {
	   no = strtoul(p, NULL, 10);
	  
     }

     hex_header = '0' [xX];
     open_par = space* '(' space*;
     close_par = space* ')' space*;

     id_type = ([aAbB]);
     id_no = ([0-9]+);
     id = space* (id_type >capture_id_type) space* (id_no >capture_id_no);
     
     cmd_val = (xdigit '0');
     cmd = space* hex_header  (cmd_val >capture_cmd) ;
     
     chan_val = ('0' xdigit);
     chan = space* hex_header (chan_val >capture_chan);
     
     param_val = ([0-7] xdigit);
     param = space* hex_header (param_val  >capture_param);
	
     confline = ( open_par open_par id close_par cmd open_par chan (param)? close_par close_par)   %{res = 0;};

	
     main := confline;
	  
}%%  

%% write data;

void init_config_parser(void)
{

     %% write init;
}

int parse_config_line(char* line, struct input_conf* map)
{
     
     char *p = line;
     char *pe = p + strlen(p);
     char *eof = pe;
     int res = -1;
     uint8_t type, no;
     %% write init;
     %% write exec;

     if (!(res)) {
	  switch (type) {
	  case('b'):
	       type = JS_EVENT_BUTTON;
	       break;
	  case('a'):
	       type = JS_EVENT_AXIS;
	       break;
	  default:
	       res = -1;
	       break;
	  }
	  map->ev_id = CREATE_EV_ID(type, no);

     }
     return res;
}


void explain_configuration(struct input_conf* mapping, const size_t mapping_s)
{
     int i = 0;
     int type, number;
     struct input_conf *m;
     char* type_name;
     char* cmd_name;

     puts("Joystick Configuration:");
     
     for (i = 0; i < mapping_s; ++i) {
	  m = (mapping + i);
	  if (m == NULL)
	       return;
	  type = (m->ev_id >> 8) & 0xFF;
	  number = m->ev_id & 0xFF;
	  type_name = (type & JS_EVENT_AXIS) ? "Axis" : (type & JS_EVENT_BUTTON) ? "Button" : "undefined";
	  cmd_name = get_string_for_midi_cmd(m->cmd);

	  
	  printf("%9s: %2d   Channel: %2d  MidiCmd: %16s  Param: %d\n",
		 type_name,
		 number,
		 m->chan+1,
		 cmd_name,
		 m->param);
     }
}


static int read_line(char* buf, FILE *fp)
{
     int c, i = 0;


     while((buf[i++] = c = getc(fp)) != '\n' && i < CHAR_BUF_SIZE) {
	  if (c == EOF)
	       return c;
     }
     buf[--i] = '\0';
     
     fprintf(stderr,"CONF_LINE: %s\n", buf);

     return i;
}

static int comp_input_conf_evid(const void* ev_id, const void* input_c)
{
     const uint16_t* id = ev_id;
     const struct input_conf* c = input_c;
     return (*id - c->ev_id);
}

struct input_conf* get_conf_for_ev_id(const uint16_t id, struct input_conf *sorted_map, size_t sorted_map_s)
{
     return bsearch(&id, sorted_map, sorted_map_s, sizeof(*sorted_map), comp_input_conf_evid);
}


int read_config_file(char* path, struct input_conf* mapping, size_t mapping_s)
{
     FILE *fp;
     int eof_seen = 0;
     int line_length, line_no;
     char buf[CHAR_BUF_SIZE];
     size_t map_idx = 0;

     
     errno = 0;
     if((fp = fopen(path, "r")) == NULL) {
	  perror("read_config_file");
	  return -1;
     }

     init_config_parser();

     line_no = 0;
     while(!eof_seen) {
	  if(!(map_idx < mapping_s)) {
	       fprintf(stderr,
		       "CONFIG_WARN> joystick supports only %zu inputs, maybe some configuration is skipped\n",
		       mapping_s);
	       break;
	  }
	  ++line_no;
	  if ((line_length = read_line(buf, fp)) == EOF) {
	       eof_seen = 1;
	       break;
	  }

	  if (line_length < 1 || buf[0] == '#')
	       continue;

	  memset(&mapping[map_idx], 0, sizeof(struct input_conf));
	  if (parse_config_line(buf, &mapping[map_idx]) != 0){
	       fprintf(stderr, "CONFIG_ERROR> line: %d :: %s\n", line_no, buf);
	       continue;
	  }
	  ++map_idx;
     }


     qsort(mapping, map_idx, sizeof(*mapping), comp_input_conf_evid);
     
     
     fclose(fp);
     return map_idx;
	  
}

