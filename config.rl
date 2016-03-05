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
	
     confline = ( open_par open_par id close_par open_par cmd  chan (param)? close_par close_par)   %{res = 0;};

	
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





static int read_till_sep(char *buf, FILE* fp, char sep, int* eof, int from)
{
     int c, i = from;
     *eof = 0;
     while ((buf[i++] = c = getc(fp)) != sep && i < CHAR_BUF_SIZE) {
	  if (c == EOF) {
	       *eof = 1;
	       break;
	  }
     }
     buf[--i] = '\0';
     return i;
}


static int read_config_line(char* buf, FILE *fp, int *eof)
{
     *eof = 0;
     int n_pos = 0;
     char *p;
     while(!*eof) {
	  if (!( strlen(buf) < CHAR_BUF_SIZE))
	       return -1;
	  
	  read_till_sep(buf, fp, '\n', eof, n_pos);

	  if (strchr(buf, '#') != NULL) {
	       /* line contains comment -> skip */
	       continue;
	  }
	  if ((p = strchr(buf, ';')) != NULL) {
	       /* config seperator found -> done */
	       *p = '\0';
	       break;
	  }
	  n_pos = strlen(buf);

     }

     return strlen(buf);;
}

static int replace_char(char* buf, char* t, char n)
{
     char *p = NULL;
     int cnt = 0;
     for (int i = 0; i < strlen(t); ++i) {
	  while((p = strchr(buf, t[i])) != NULL) {
	       ++cnt;
	       *p = n;
	  }
     }
     return cnt;
}

static int multiline_to_singleline(char* buf)
{
     return replace_char(buf, "\n\t", ' ');
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
     int conf_length, n_lines, line_no;
     char buf[CHAR_BUF_SIZE];
     size_t map_idx = 0;
     

     
     errno = 0;
     if((fp = fopen(path, "rb")) == NULL) {
	  perror("read_config_file");
	  return -1;
     }

     init_config_parser();

     line_no = 1;
     while(!eof_seen) {

	  if(!(map_idx < mapping_s)) {
	       fprintf(stderr,
		       "CONFIG_WARN> joystick supports only %zu inputs, maybe some configuration is skipped\n",
		       mapping_s);
	       break;
	  }
	  memset(buf, '\0', CHAR_BUF_SIZE);
	  if((conf_length = read_config_line(buf, fp, &eof_seen)) < 1)
	       continue;
	  if(eof_seen)
	       break;

	  if (strchr(buf, '#') != NULL)
	       continue;
	  
	  if ((n_lines = multiline_to_singleline(buf)) == 0)
	       n_lines = 1;


	  
	  fprintf(stderr,"CONFIG_ENTRY %d: %s\n", line_no, buf);


	  
	  line_no += n_lines;	  



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

