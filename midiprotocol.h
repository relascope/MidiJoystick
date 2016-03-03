#ifndef MIDIPROTOCOL_H
#define MIDIPROTOCOL_H

/**
 * http://www.zem-college.de/midi/index.htm
 * https://ccrma.stanford.edu/~craig/articles/linuxmidi/misc/essenmidi.html
 *
 * Midi Bytes range from 0 - 255 (0x0 - 0xFF)
 *
 * Data Bytes range from 0 - 127 (0x0 - 0x7F)
 *
 * CMD Bytes range from 128 - 255 (0x80 - 0xFF)
 * MSB(CMD) = actual command
 * LSB(CMD) = channel
 * eg: 0x91 = Note on for channel 2
 *
 * CMD Reference:
 *
 * 0x80     Note Off
 * 0x90     Note On
 * 0xA0     Aftertouch
 * 0xB0     Continuous controller
 * 0xC0     Patch change
 * 0xD0     Channel Pressure
 * 0xE0     Pitch bend
 * 0xF0     (non-musical commands up to 0xFF 'System Messages')
 *
 *
 * MIDI MESSAGES:
 * minimum size is 1byte (one CMD byte and no parameter bytes)
 * maximum size is 3bytes (considering 0xF0 commands)
 *
 * Message starts always with CMD byte
 * 
 * Command 	Meaning 	        # parameters 	param 1 	param 2
 * 0x80   	Note-off 	        2 	        key 	        velocity
 * 0x90	        Note-on 	        2 	        key 	        veolcity
 * 0xA0	        Aftertouch 	        2 	        key 	        touch
 * 0xB0	        Continuous controller 	2 	        controller # 	controller value
 * 0xC0	        Patch change 	        1 	        instrument # 	
 * 0xD0	        Channel Pressure 	1 	        pressure
 * 0xE0	        Pitch bend 	        2 	        lsb (7 bits) 	msb (7 bits)
 * 0xF0	        (non-musical commands) 			
 *
 * command 	meaning 	                           # param
 * 0xF0 	start of system exclusive message 	   variable
 * 0xF1 	MIDI Time Code Quarter Frame (Sys Common)	
 * 0xF2 	Song Position Pointer (Sys Common)	
 * 0xF3 	Song Select (Sys Common) 	
 * 0xF4 	??? 	
 * 0xF5 	??? 	
 * 0xF6 	Tune Request (Sys Common) 	
 * 0xF7 	end of system exclusive message 	   0
 * 0xF8 	Timing Clock (Sys Realtime) 	
 * 0xFA 	Start (Sys Realtime) 	
 * 0xFB 	Continue (Sys Realtime) 	
 * 0xFC 	Stop (Sys Realtime) 	
 * 0xFD 	??? 	
 * 0xFE 	Active Sensing (Sys Realtime) 	
 * 0xFF 	System Reset (Sys Realtime) 
 */

#include <inttypes.h>

#define MIDI_MAX_LENGTH 3


struct midi_msg {
     uint8_t midi_buf[MIDI_MAX_LENGTH];
     uint8_t size;
};




enum midi_commands {
     midi_command_note_off = 0x80,
     midi_command_note_on = 0x90,
     midi_command_aftertouch = 0xA0,
     midi_command_cc = 0xB0,
     midi_command_patch_change = 0xC0,
     midi_command_channel_pressure = 0xD0,
     midi_command_pitch_bend = 0xE0,
     midi_command_sysex = 0XF0
};

char* get_string_for_midi_cmd(enum midi_commands cmd);

uint8_t map_to_midi_range(long min, long max, long val);

void map_to_midi_pitchbend_range(long min, long max, uint16_t val, uint8_t *lsb, uint8_t* msb);

//typedef int (*_create_midi_msg)(uint8_t, uint8_t*, size_t, struct midi_msg*);



/**
 * Functions return byte string in result, sizeof bytestring as return value 
 */
int create_midi_note_off(uint8_t channel, uint8_t note, uint8_t velocity, struct midi_msg* result);
int create_midi_note_on(uint8_t channel, uint8_t note, uint8_t velocity, struct midi_msg* result);
int create_midi_cc(uint8_t channel, uint8_t cc_id, uint8_t cc_val, struct midi_msg* result);
int create_midi_aftertouch(uint8_t channel, uint8_t note, uint8_t touch, struct midi_msg* result);
int create_midi_patch_change(uint8_t channel, uint8_t patch_id, struct midi_msg* result);
int create_midi_channel_pressure(uint8_t channel, uint8_t pressure, struct midi_msg* result);
int create_midi_pitch_bend(uint8_t channel, uint8_t lsb, uint8_t msb, struct midi_msg* result);

#endif
