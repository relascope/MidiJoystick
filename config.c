
#line 1 "config.rl"
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


#line 72 "config.rl"
  


#line 24 "config.c"
static const char _conf_parser_actions[] = {
	0, 1, 0, 1, 1, 1, 2, 1, 
	3, 1, 4, 1, 5
};

static const char _conf_parser_key_offsets[] = {
	0, 0, 4, 8, 15, 20, 26, 30, 
	34, 38, 40, 46, 47, 51, 53, 54, 
	60, 65, 69, 71, 73, 79, 83
};

static const char _conf_parser_trans_keys[] = {
	32, 40, 9, 13, 32, 40, 9, 13, 
	32, 9, 13, 65, 66, 97, 98, 32, 
	9, 13, 48, 57, 32, 41, 9, 13, 
	48, 57, 32, 41, 9, 13, 32, 40, 
	9, 13, 32, 48, 9, 13, 88, 120, 
	48, 57, 65, 70, 97, 102, 48, 32, 
	48, 9, 13, 88, 120, 48, 48, 57, 
	65, 70, 97, 102, 32, 41, 48, 9, 
	13, 32, 41, 9, 13, 88, 120, 48, 
	55, 48, 57, 65, 70, 97, 102, 32, 
	41, 9, 13, 32, 9, 13, 0
};

static const char _conf_parser_single_lengths[] = {
	0, 2, 2, 1, 1, 2, 2, 2, 
	2, 2, 0, 1, 2, 2, 1, 0, 
	3, 2, 2, 0, 0, 2, 1
};

static const char _conf_parser_range_lengths[] = {
	0, 1, 1, 3, 2, 2, 1, 1, 
	1, 0, 3, 0, 1, 0, 0, 3, 
	1, 1, 0, 1, 3, 1, 1
};

static const char _conf_parser_index_offsets[] = {
	0, 0, 4, 8, 13, 17, 22, 26, 
	30, 34, 37, 41, 43, 47, 50, 52, 
	56, 61, 65, 68, 70, 74, 78
};

static const char _conf_parser_indicies[] = {
	0, 2, 0, 1, 2, 3, 2, 1, 
	3, 3, 4, 4, 1, 5, 5, 6, 
	1, 7, 8, 7, 9, 1, 7, 8, 
	7, 1, 8, 10, 8, 1, 10, 11, 
	10, 1, 12, 12, 1, 13, 13, 13, 
	1, 14, 1, 14, 15, 14, 1, 16, 
	16, 1, 17, 1, 18, 18, 18, 1, 
	18, 19, 20, 18, 1, 19, 21, 19, 
	1, 22, 22, 1, 23, 1, 24, 24, 
	24, 1, 24, 19, 24, 1, 21, 21, 
	1, 0
};

static const char _conf_parser_trans_targs[] = {
	1, 0, 2, 3, 4, 4, 5, 6, 
	7, 5, 8, 9, 10, 11, 12, 13, 
	14, 15, 16, 17, 18, 22, 19, 20, 
	21
};

static const char _conf_parser_trans_actions[] = {
	0, 0, 0, 0, 7, 0, 9, 0, 
	0, 0, 0, 0, 0, 5, 0, 0, 
	0, 3, 0, 0, 0, 0, 0, 1, 
	0
};

static const char _conf_parser_eof_actions[] = {
	0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 0, 0, 
	0, 0, 0, 0, 0, 0, 11
};

static const int conf_parser_start = 1;
static const int conf_parser_first_final = 22;
static const int conf_parser_error = 0;

static const int conf_parser_en_main = 1;


#line 75 "config.rl"

void init_config_parser(void)
{

     
#line 115 "config.c"
	{
	cs = conf_parser_start;
	}

#line 80 "config.rl"
}

int parse_config_line(char* line, struct input_conf* map)
{
     
     char *p = line;
     char *pe = p + strlen(p);
     char *eof = pe;
     int res = -1;
     uint8_t type, no;
     
#line 132 "config.c"
	{
	cs = conf_parser_start;
	}

#line 91 "config.rl"
     
#line 139 "config.c"
	{
	int _klen;
	unsigned int _trans;
	const char *_acts;
	unsigned int _nacts;
	const char *_keys;

	if ( p == pe )
		goto _test_eof;
	if ( cs == 0 )
		goto _out;
_resume:
	_keys = _conf_parser_trans_keys + _conf_parser_key_offsets[cs];
	_trans = _conf_parser_index_offsets[cs];

	_klen = _conf_parser_single_lengths[cs];
	if ( _klen > 0 ) {
		const char *_lower = _keys;
		const char *_mid;
		const char *_upper = _keys + _klen - 1;
		while (1) {
			if ( _upper < _lower )
				break;

			_mid = _lower + ((_upper-_lower) >> 1);
			if ( (*p) < *_mid )
				_upper = _mid - 1;
			else if ( (*p) > *_mid )
				_lower = _mid + 1;
			else {
				_trans += (unsigned int)(_mid - _keys);
				goto _match;
			}
		}
		_keys += _klen;
		_trans += _klen;
	}

	_klen = _conf_parser_range_lengths[cs];
	if ( _klen > 0 ) {
		const char *_lower = _keys;
		const char *_mid;
		const char *_upper = _keys + (_klen<<1) - 2;
		while (1) {
			if ( _upper < _lower )
				break;

			_mid = _lower + (((_upper-_lower) >> 1) & ~1);
			if ( (*p) < _mid[0] )
				_upper = _mid - 2;
			else if ( (*p) > _mid[1] )
				_lower = _mid + 2;
			else {
				_trans += (unsigned int)((_mid - _keys)>>1);
				goto _match;
			}
		}
		_trans += _klen;
	}

_match:
	_trans = _conf_parser_indicies[_trans];
	cs = _conf_parser_trans_targs[_trans];

	if ( _conf_parser_trans_actions[_trans] == 0 )
		goto _again;

	_acts = _conf_parser_actions + _conf_parser_trans_actions[_trans];
	_nacts = (unsigned int) *_acts++;
	while ( _nacts-- > 0 )
	{
		switch ( *_acts++ )
		{
	case 0:
#line 21 "config.rl"
	{
//	  printf("para: %s\n", p);
	  map->param = (strtoul(p, NULL, 16) & 0x7F);
     }
	break;
	case 1:
#line 26 "config.rl"
	{
//	  printf("cha %s\n", p);
	  map->chan = (strtoul(p, NULL, 16) & 0xF);
     }
	break;
	case 2:
#line 31 "config.rl"
	{
//	  printf("cmd %s\n", p);
	  map->cmd = (strtoul(p, NULL, 16) & 0xF0);
	  /*
	  map->cmd = (strtoul(p, NULL, 16) & 0xFF);
	  if (!((map->cmd & 0xF0) == 0xF0))
	       map->cmd = map->cmd & 0xF0;
	  */
     }
	break;
	case 3:
#line 41 "config.rl"
	{
	  type =  tolower(*p);

     }
	break;
	case 4:
#line 45 "config.rl"
	{
	   no = strtoul(p, NULL, 10);
	  
     }
	break;
#line 253 "config.c"
		}
	}

_again:
	if ( cs == 0 )
		goto _out;
	if ( ++p != pe )
		goto _resume;
	_test_eof: {}
	if ( p == eof )
	{
	const char *__acts = _conf_parser_actions + _conf_parser_eof_actions[cs];
	unsigned int __nacts = (unsigned int) *__acts++;
	while ( __nacts-- > 0 ) {
		switch ( *__acts++ ) {
	case 5:
#line 67 "config.rl"
	{res = 0;}
	break;
#line 273 "config.c"
		}
	}
	}

	_out: {}
	}

#line 92 "config.rl"

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

