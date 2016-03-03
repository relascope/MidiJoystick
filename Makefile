CC = cc
#CFLAGS += -std=c11 -D_XOPEN_SOURCE=600 -pedantic -Wall -W -g
CFLAGS += -std=c11 -D_XOPEN_SOURCE=600 -Wall -g -O0
LDFLAGS = $(shell pkg-config --cflags --libs jack) -lm
#LDFLAGS = -lm

all: midijoystick explain_config config.pdf


explain_config: config.o explain_config.o midiprotocol.o joystick.o

midijoystick: midijoystick.o joystick.o midijack.o midiprotocol.o mapping.o config.o

midijoystick.o: main.o
	$(shell cp main.o midijoystick.o)

main.o: main.c

mapping.o: mapping.c

joystick.o: joystick.c joystick.h

midijack.o: midijack.c midijack.h

midiprotocol.o: midiprotocol.c midiprotocol.h

explain_config.o: explain_config.c

config.o: config.c config.h

config.c: config.rl
	ragel -o config.c -C config.rl

.PHONY: config.pdf
config.pdf:
	ragel -Vp -o config.dot config.rl
	dot -Tpdf -o config.pdf config.dot 

.PHONY: purge
purge:
	rm *.o config.dot config.pdf midijoystick explain_config config.c

.PHONY: clean
clean:
	rm *.o midijoystick explain_config





