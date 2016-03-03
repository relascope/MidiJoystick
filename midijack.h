#ifndef MIDIJACK_H
#define MIDIJACK_H

#include <jack/ringbuffer.h>

int setup_jack(jack_ringbuffer_t** ringbuf, size_t ringbuf_size );
int tear_down_jack(void);

#endif
