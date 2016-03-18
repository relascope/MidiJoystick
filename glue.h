#ifndef GLUE_H
#define GLUE_H

#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>
#include <string.h>


#define MAX_MIDI_MSG 256
//#define MAX_MIDI_MSG 5

struct midi_msg {
     uint8_t msg[MAX_MIDI_MSG];
     size_t size;
};

#define DEFAULT_RB_SIZE ((16384 * sizeof(struct midi_msg)))

#endif
