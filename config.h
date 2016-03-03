#ifndef CONFIG_H
#define CONFIG_H

#include <stdint.h>

struct input_conf {
     uint16_t ev_id;
     int16_t last_value;
     uint8_t cmd;
     uint8_t chan;
     uint8_t param;
};

#define CREATE_EV_ID(t, n) ( (((t) << 8) & 0xFF00) | ((n) & 0xFF) )

void init_config_parser(void);

int parse_config_line(char* line, struct input_conf* map);

struct input_conf* get_conf_for_ev_id(const uint16_t id, struct input_conf *sorted_map, size_t sorted_map_s);

int read_config_file(char* path, struct input_conf* mapping, size_t mapping_s);

void explain_configuration(struct input_conf* mapping, size_t mapping_s);

#endif
