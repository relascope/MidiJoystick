#include <unistd.h>
#include <stdlib.h>
#include <math.h>
#include <linux/joystick.h>
#include <stdio.h>

#include "midiprotocol.h"
#include "joystick.h"
#include "mapping.h"
#include "config.h"

static long scale_value_to_range(long val, long val_min, long val_max, long out_min, long out_max)
{
     if (val < val_min || val > val_max)
	  return 0;
     double slope = 1.0 * (out_max - out_min) / (val_max - val_min);
     return floor((out_min + slope * (val - val_min)) + 0.5);
}

static long scale_to_positive_axis(long val)
{
     return scale_value_to_range(val, AXIS_REAL_MIN, AXIS_REAL_MAX, AXIS_POS_MIN, AXIS_POS_MAX);
}

#define AXIS_MIDI_NORM 0x40
int get_midi_msg(struct js_event* ev, struct input_conf* input_map, size_t input_map_s, struct midi_msg* midi)
{
     struct input_conf* im = NULL;
     int32_t axis = 0;
     uint8_t midi_val = 0;
     
     if (ev->type & JS_EVENT_INIT)
	  return -1;

     uint16_t ev_id = CREATE_EV_ID(ev->type, ev->number);

     im = get_conf_for_ev_id(ev_id, input_map, input_map_s);
     
     if (!im) {
	  fprintf(stderr, "joystick event (%s %2d) is not configured\n",
		  (ev->type & JS_EVENT_AXIS) ? "A" : (ev->type & JS_EVENT_BUTTON) ? "B" : "undefined",
		  ev->number);
	  return -1;
     }


     /* do nothing if value is same */
     if (ev->value == im->last_value) {
	  return -1;
     }
     im->last_value = ev->value;
     
     axis = ev->value;



     
     
     /* transpose buttons to axis values */
     if (ev->type == JS_EVENT_BUTTON) {
	  switch (axis) {
	  case(EVENT_BTN_PRESSED):
	       axis = AXIS_REAL_MAX;
	       break;
	  case(EVENT_BTN_RELEASED):
	       axis = AXIS_REAL_MIN;
	       break;
	  default:
	       fprintf(stderr, "button event %X not understood\n", ev->value);
	       return -1;
	  }
     }
     

     /* create deadzone */
     if (axis < 4000 && axis > -4000)
	  axis = 0;

     /* scale to positive range */
     axis = scale_to_positive_axis(axis);


     /* pitchbend is special */
     if (im->cmd == midi_command_pitch_bend) {
	  uint8_t lsb, msb;
	  map_to_midi_pitchbend_range(AXIS_POS_MIN, AXIS_POS_MAX, axis, &lsb, &msb);
	  create_midi_pitch_bend(im->chan, lsb, msb, midi);
	  return 0;
     }
	  

     /* scale positive value to midi range */
     midi_val = map_to_midi_range(AXIS_POS_MIN, AXIS_POS_MAX, axis);
     midi_val &= 0x7F;

     switch (im->cmd) {
     case (midi_command_cc):
	  create_midi_cc(im->chan, im->param, midi_val, midi);
	  break;
     case (midi_command_patch_change):
	  create_midi_patch_change(im->chan, im->param, midi);
	  break;
     case (midi_command_channel_pressure):
	   create_midi_channel_pressure(im->chan, midi_val, midi);
	   break;
     case (midi_command_note_on):
	  midi_val = midi_val - (AXIS_MIDI_NORM - im->param);
	  create_midi_note_on(im->chan, midi_val, 0x7F, midi);
	  break;
     case (midi_command_sysex):
	  fprintf(stderr, "Midi SysEx is currently not supported\n");
	  return -1;
	  break;
     case (midi_command_note_off):
	  fprintf(stderr, "Midi Note Off is currently not supported, use Note On instead\n");
	  return -1;
	  break;
     case (midi_command_aftertouch):
	   fprintf(stderr, "Midi Aftertouch is currently not supported\n");
	   return -1;
	   break;
     default:
	  fprintf(stderr, "Midi Command %X not understood\n", im->cmd);
	  return -1;
     }

     
     return 0;
}





 
#if 0
int translate_input_to_midi(struct js_event* ev, struct midi_msg* midi)
{
     uint8_t lsb, msb;
     long axis;
     
     if (ev->type & JS_EVENT_INIT)
	  return -1;

     if (ev->type == JS_EVENT_BUTTON) {
	  fprintf(stderr, "No button actions defined\n");
	  return -1;
     }

     
     if (ev->type == JS_EVENT_AXIS) {
	  /* create deadzone */
	  if (ev->value < 4000 && ev->value > -4000)
	       ev->value = 0;
	  
	  switch (ev->number) {
	  case xbox_input_axis_y_left_as:
	       axis = get_positive_axis(ev->value);
	       map_to_midi_pitchbend_range(AXIS_POS_MIN, AXIS_POS_MAX, axis, &lsb, &msb);
	       create_midi_pitch_bend(MIDI_CHANNEL, lsb, msb, midi);
	       return 0;
	       break;
	  default:
	       fprintf(stderr, "Axis number %d is not defined\n", ev->number);
	       return -1;
	       break;
	  }
     }
     return 0;
}
#endif
