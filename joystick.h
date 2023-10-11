#ifndef JOYSTICK_H
#define JOYSTICK_H

/*************************************************************** 

https://www.kernel.org/doc/Documentation/input/joystick.txt
https://www.kernel.org/doc/Documentation/input/input.txt
https://www.kernel.org/doc/Documentation/input/gamepad.txt
is linux/joystick.h depricated? 
Based on https://www.kernel.org/doc/Documentation/input/joystick-api.txt

struct js_event {
     uint32_t time;   // rawEvent timestamp in milliseconds
     int16_t value;   // value 
     uint8_t type;    // rawEvent type
     uint8_t number;  // axis/button number
};

#define JS_EVENT_BUTTON 0x01  // button pressed/released
#define JS_EVENT_AXIS 0x02    // joystick moved
#define JS_EVENT_INIT 0x80    // initial state of device

***************************************************************/

#include <linux/joystick.h>

#ifdef __cplusplus
extern "C" {
#endif

#define EVENT_BTN_PRESSED 0x1
#define EVENT_BTN_RELEASED 0x0

extern int open_joystick(char*);

extern int close_joystick(int);

extern int get_joystick_event(int fd, struct js_event *);

extern void debug_print_joystick_event(const struct js_event * const);

extern int get_number_of_axes(int fd);

extern int get_number_of_buttons(int fd);

#ifdef __cplusplus
}
#endif

#endif
