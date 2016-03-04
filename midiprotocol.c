#include <stdlib.h>
#include <math.h>
#include "midiprotocol.h"

#define MIDI_NAME_SIZE 1027
struct midi_command_name {
     uint8_t id;
     char name[MIDI_NAME_SIZE];
};

static int comp_midi_command_name(const void* midi_command, const void* midi_cmd_name)
{
     uint8_t* id = (uint8_t*) midi_command;
     struct midi_command_name* name = (struct midi_command_name*) midi_cmd_name;
     return (*id - name->id);
}

char* get_string_for_midi_cmd(enum midi_commands cmd)
{
     /**
      * Presorted Lookuptable
      */
     struct midi_command_name names[] = {
	  {.id = midi_command_note_off, .name = "Note Off\0"},
	  {.id = midi_command_note_on, .name = "Note On\0"},
	  {.id = midi_command_aftertouch, .name = "Aftertouch\0"},
	  {.id = midi_command_cc, .name = "CC\0"},
	  {.id = midi_command_patch_change, .name = "Patch Change\0"},
	  {.id = midi_command_channel_pressure, .name = "Channel Pressure\0"},
	  {.id = midi_command_pitch_bend, .name = "Pitch Bend\0"},
	  {.id = midi_command_sysex, .name = "SysEx\0"},
     };
     cmd &= 0xF0;
     struct midi_command_name *name = NULL;
     name = bsearch(&cmd, names, sizeof(names)/sizeof(struct midi_command_name),
			 sizeof(struct midi_command_name), comp_midi_command_name);
     if (!(name))
	  return "undefined";
     return name->name;
	      
}

#define MIDI_DATA_MIN 0x00
#define MIDI_DATA_MAX 0x7F
uint8_t map_to_midi_range(long min, long max, long val)
{
     double slope = 1.0 * (MIDI_DATA_MAX - MIDI_DATA_MIN) / (max - min);
     return ((int)floor((MIDI_DATA_MIN + slope * (val - min)) + 0.5) & MIDI_DATA_MAX);
}

#define MIDI_PITCH_BEND_MAX 0x3FFF
void map_to_midi_pitchbend_range(long min, long max, uint16_t val, uint8_t *lsb, uint8_t* msb)
{
     double slope = 1.0 * (MIDI_PITCH_BEND_MAX - MIDI_DATA_MIN) / (max - min);
     val = floor((MIDI_DATA_MIN + slope * (val - min)) + 0.5);
     *lsb = (val & 0x7F);
     *msb = (val >> 7) & 0x7F;
     return;
}
     
     

static inline uint8_t build_cmd_byte(enum midi_commands cmd, uint8_t channel)
{
     channel = 0x0F & channel;
     cmd = ((uint8_t) cmd) | channel;
     return cmd;
}

static inline struct midi_msg* build_3_byte_message(enum midi_commands cmd, uint8_t channel, uint8_t param1,
					    uint8_t param2, struct midi_msg* result)
{
     result->midi_buf[0] = build_cmd_byte(cmd, channel);
     result->midi_buf[1] = MIDI_DATA_MAX & param1;
     result->midi_buf[2] = MIDI_DATA_MAX & param2;
     result->size = 3;
     return result;
}

static inline struct midi_msg* build_2_byte_message(enum midi_commands cmd, uint8_t channel, uint8_t param1,
					    struct midi_msg* result)
{
     result->midi_buf[0] = build_cmd_byte(cmd, channel);
     result->midi_buf[1] = MIDI_DATA_MAX & param1;
     result->size = 2;
     return result;
}


int create_midi_note_off(uint8_t channel, uint8_t note, uint8_t velocity, struct midi_msg* result)
{
     if (!(result = build_3_byte_message(midi_command_note_off, channel, note, velocity, result)))
	  return -1;
     return 0;
}

int create_midi_note_on(uint8_t channel, uint8_t note, uint8_t velocity, struct midi_msg* result)
{
     if (!(result = build_3_byte_message(midi_command_note_on, channel, note, velocity, result)))
	  return -1;
     return 0;
}

#define CHANNEL_MODE_MUTE_1 0x78
#define CHANNEL_MODE_RESET 0x79
#define CHANNEL_MODE_LOCAL 0x7A
#define CHANNEL_MODE_MUTE_2 0x7B
#define CHANNEL_MODE_OMNI_ON 0x7C
#define CHANNEL_MODE_OMNI_OFF 0x7D
#define CHANNEL_MODE_MONO 0x7E
#define CHANNEL_MODE_POLY 0x7F

int create_midi_cc(uint8_t channel, uint8_t cc_id, uint8_t cc_val, struct midi_msg* result)
{
     switch (cc_id) {
	  /* these Channel Mode messages have 0x00 as their parameter */
     case (CHANNEL_MODE_MUTE_1):
     case (CHANNEL_MODE_MUTE_2):
     case (CHANNEL_MODE_OMNI_OFF):
     case (CHANNEL_MODE_OMNI_ON):
     case (CHANNEL_MODE_RESET):
	  cc_val = 0x00;
	  break;
     default:
	  break;
     }

     if (!(result = build_3_byte_message(midi_command_cc, channel, cc_id, cc_val, result)))
	  return -1;
     return 0;
}

int create_midi_aftertouch(uint8_t channel, uint8_t note, uint8_t touch, struct midi_msg* result)
{
     if(!(result = build_3_byte_message(midi_command_aftertouch, channel, note, touch, result)))
	  return -1;
     return 0;
}

int create_midi_patch_change(uint8_t channel, uint8_t patch_id, struct midi_msg* result)
{
     if(!(result = build_2_byte_message(midi_command_patch_change, channel, patch_id, result)))
	  return -1;
     return 0;
}

int create_midi_channel_pressure(uint8_t channel, uint8_t pressure, struct midi_msg* result)
{
     if(!(result = build_2_byte_message(midi_command_channel_pressure, channel, pressure, result)))
	  return -1;
     return 0;
}

int create_midi_pitch_bend(uint8_t channel, uint8_t lsb, uint8_t msb, struct midi_msg* result)
{
     if(!(result = build_3_byte_message(midi_command_pitch_bend, channel, lsb, msb, result)))
	  return -1;
     return 0;
}
