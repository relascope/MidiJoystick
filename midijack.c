#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <jack/jack.h>
#include <jack/types.h>
#include <jack/midiport.h>
#include <jack/session.h>
#include <jack/ringbuffer.h>

#include "midiprotocol.h"

#define CLIENT_NAME "MIDI_JOYSTICK"
#define OUTPUT_PORT_NAME "MidiOut"


static jack_port_t *output_port;
static jack_client_t *client;
static jack_ringbuffer_t *rb;

/**
 * signal handler needed to shutdown client properly
 */
static void signal_handler(int sig)
{
     
     jack_ringbuffer_free (rb);
     jack_client_close(client);
     fprintf(stderr, "received signal, exiting... \n");
     exit(0);
}


/**
 * shutdown callback called by JACK if server shuts down or disconnects this client
 */
static void jack_shutdown(void *arg)
{
     exit(1);
}

/**
 * process callback called by JACK in special realtime thread once for each audio cycle
 */
int process (jack_nframes_t nframes, void *arg)
{

     void* port_buf = jack_port_get_buffer(output_port, nframes);
     struct midi_msg midi;
     jack_nframes_t frame_idx;
     jack_midi_data_t midi_data[MIDI_MAX_LENGTH];
     int data_idx;



     jack_midi_clear_buffer(port_buf);

     for (frame_idx = 0; frame_idx < nframes; ++frame_idx) {

	  if (jack_ringbuffer_read(rb, (void*) &midi, sizeof(midi)) != sizeof(midi)) {
	       /* unable to get full midi message -> skip */
	       continue;
	  }
	  for (data_idx = 0; data_idx < midi.size; ++data_idx) {
	       midi_data[data_idx] = midi.midi_buf[data_idx];
	  }
	       
	  jack_midi_event_write(port_buf, frame_idx, midi_data, sizeof(midi_data));
     }


     return 0;

}

int setup_jack(jack_ringbuffer_t** ringbuf, size_t ringbuf_size)
{


     if ((rb = jack_ringbuffer_create(ringbuf_size)) == NULL) {
	  fprintf(stderr, "failed to create jack ringbuffer\n");
	  return -1;
     }
     *ringbuf = rb;
     if ((client = jack_client_open(CLIENT_NAME, JackNullOption, NULL)) == NULL) {
	  fprintf(stderr, "failed to connect to JACK Server\n");
	  return -1;
     }

     if (jack_set_process_callback(client, process, 0) != 0) {
	  fprintf(stderr, "failed to register process callback\n");
	  return -1;
     }

     jack_on_shutdown(client, jack_shutdown, 0);


	  

     /* When JACK is running realtime, jack_activate() will have
      * called mlockall() to lock our pages into memory.  But, we
      * still need to touch any newly allocated pages before
      * process() starts using them.  Otherwise, a page fault could
      * create a delay that would force JACK to shut us down. */

     memset(rb->buf, 0, rb->size);

     if (!(output_port = jack_port_register(client, OUTPUT_PORT_NAME, JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0))) {
	  fprintf(stderr, "failed to register output port \n");
	  return -1;
     }

     /* register signal handlers for proper shutdown */
     signal(SIGTERM, signal_handler);
     signal(SIGINT, signal_handler);

     if (jack_activate(client)) {
	  fprintf(stderr, "cannot activate client\n");
	  return -1;
     }
     

     
     return 0;
}

int tear_down_jack(void)
{
     
     jack_ringbuffer_free(rb);
     jack_client_close(client);
     return 0;
}
