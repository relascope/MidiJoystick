#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
//#include <jack/jack.h>
#include <jack/ringbuffer.h>

#include "midijack.h"
#include "midiprotocol.h"
#include "joystick.h"
#include "mapping.h"
#include "config.h"



#define DEVICE_PATH "/dev/input/js0"
#define CONFIG_PATH "./input.conf"


#define DEFAULT_RB_SIZE 16384		/* ringbuffer size in frames */



int main(int argc, char* argv[])
{
     int js_fd, rs;
     int n_axes, n_btns, n_inputs;
     jack_ringbuffer_t *rb = NULL;

     struct input_conf *tmp_map = NULL;
     struct js_event ev;
     struct midi_msg midi;
     size_t n_inputs_configured;

     js_fd = open_joystick(DEVICE_PATH);

     if ((n_axes = get_number_of_axes(js_fd)) == -1)
	  n_axes = 0;
     if ((n_btns = get_number_of_buttons(js_fd)) == -1)
	  n_btns = 0;

     n_inputs = n_btns + n_axes;
     if (n_inputs < 1) {
	  fprintf(stderr, "joystick seems to have no axes or buttons\n");
	  return EXIT_FAILURE;
     }

     errno = 0;
     if (!(tmp_map = calloc(n_inputs, sizeof(*tmp_map)))) {
	  perror("failed to allocate memory\n");
	  return EXIT_FAILURE;
     }
     
     n_inputs_configured = read_config_file(CONFIG_PATH, tmp_map, n_inputs);
     if (n_inputs_configured < 1) {
	  fprintf(stderr, "no inputs are configured\n");
	  return EXIT_FAILURE;
     }

     struct input_conf mapping[sizeof(*tmp_map) * n_inputs_configured];
     memcpy(mapping, tmp_map, sizeof(*tmp_map) * n_inputs_configured);
     free(tmp_map);
     explain_configuration(mapping, n_inputs_configured);


     if (setup_jack(&rb, DEFAULT_RB_SIZE * MIDI_MAX_LENGTH)) {
	  return EXIT_FAILURE;
     }

     if (rb == NULL) {
	  fprintf(stderr, "ringbuffer is not initalized\n");
	  return EXIT_FAILURE;
     }

     while(1) {
	  memset(&ev, 0, sizeof(ev));  
	  rs = get_joystick_event(js_fd, &ev);
	  if (rs != 0) continue;
	  //debug_print_joystick_event(&ev);
	  memset(&midi, 0, sizeof(midi));

	    
	  if (get_midi_msg(&ev, mapping, n_inputs_configured, &midi)) {
	       continue;
	  }

	  if (jack_ringbuffer_write_space(rb) < sizeof(midi)) {
	       fprintf(stderr, "midi message will not fit in ringbuffer\n");
	       continue;
	  }

	  
	  if (jack_ringbuffer_write(rb, (void*) &midi, sizeof(midi)) != sizeof(midi)) {
	       fprintf(stderr, "unable to write whole midi message\n");
	       continue;
	  }
	  

	  printf("CMD: %X LSB: %X MSB: %X\n\n", midi.midi_buf[0], midi.midi_buf[1], midi.midi_buf[2]);
	    
		 
     }

     close_joystick(js_fd);
     return EXIT_SUCCESS;

}
