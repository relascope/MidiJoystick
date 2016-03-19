
all: midijoystick

midijoystick:  midijack.c joystick.c  midijoystick.scm
	gsc-script -debug -warnings -verbose -cc-options "-std=c11 -Wall -g -O0"  -ld-options $(shell pkg-config --cflags --libs jack)  -exe -o midijoystick $+






.PHONY: clean
clean:
	rm *.o midijoystick





