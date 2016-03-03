#ifndef MAPPING_H
#define MAPPING_H

#include <linux/joystick.h>
#include "config.h"

#define MIDI_CHANNEL 0x0

enum xbox_input_btns {
     xbox_input_btn_a = 0x0,
     xbox_input_btn_b = 0x1,
     xbox_input_btn_x = 0x2,
     xbox_input_btn_y = 0x3,
     xbox_input_lb = 0x4,
     xbox_input_rb = 0x5,
     xbox_input_btn_select = 0x6,
     xbox_input_btn_start = 0x7,
     xbox_input_btn_mode = 0x8,
     xbox_input_btn_left_as = 0x9,
     xbox_input_btn_right_as = 0xA
};

enum xbox_input_axes {
     xbox_input_axis_x_left_as = 0x0,
     xbox_input_axis_y_left_as = 0x1,
     xbox_input_axis_left_trigger = 0x2,
     xbox_input_axis_x_right_as = 0x3,
     xbox_input_axis_y_right_as = 0x4,
     xbox_input_axis_right_trigger = 0x5,
     xbox_input_axis_x_dpad = 0x6,
     xbox_input_axis_y_dpad = 0x7
};

#define BTN_EVENT_ON 0x1
#define BTN_EVENT_OFF 0x0
#define AXIS_REAL_MIN -32767
#define AXIS_REAL_MAX 32767
#define AXIS_POS_MAX 0xFFFF
#define AXIS_POS_MIN 0x0

long get_positive_axis(long val);

int setup_mapping(void);

int get_midi_msg(struct js_event* ev, struct input_conf* input_map, size_t input_map_s, struct midi_msg* midi);

#endif
