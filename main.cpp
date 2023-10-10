#include "joystick.h"

//#include "xx.h"

#include <cstring>
#include <iostream>


typedef int joystick_;

int main() {
    std::cout << "MidiJoystick" << std::endl;


    joystick_ js = open_joystick("/dev/input/js0");

    while (true) {
        js_event event;
        memset(&event, 0, sizeof(joystick_));

        if (get_joystick_event(js, &event) != -1) {
            debug_print_joystick_event(&event);
        }
    }

    return 0;
}
