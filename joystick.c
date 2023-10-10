#include <unistd.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
//#include <stropts.h>

#include "joystick.h"



int open_joystick(char * path)
{
     int fd;
     errno = 0;
//     if ((fd = open(path, O_RDONLY | O_NONBLOCK)) == -1) {
     if ((fd = open(path, O_RDONLY)) == -1) {
	  perror("open joystick");
	  exit(1);
     }
     return fd;
}

int close_joystick(int fd)
{
     int res;
     errno = 0;
     if ((res = close(fd)) == -1) {
	  perror("close joystick");
	  exit(1);
     }
     return res;
}

int get_number_of_axes(int fd)
{
     char axes;
     errno = 0;
     if (ioctl(fd, JSIOCGAXES, &axes)) {
	  if (errno != 0)
	       perror("get number of axes");
	  return -1;
     }
     return axes;
}

int get_number_of_buttons(int fd)
{
     char btns;
     errno = 0;
     if (ioctl(fd, JSIOCGBUTTONS, &btns)) {
	  if (errno != 0)
	       perror("get number of buttons");
	  return -1;
     }
     return btns;
	  
}

int get_joystick_event(int fd, struct js_event * event)
{
     intmax_t bytes;

     errno = 0;
     if((bytes = read(fd, event, sizeof(*event))) == -1) {
	  if (errno != EAGAIN)
	       perror("read joystick event");
	  return bytes;
     }
     if (bytes == sizeof(*event)) {
	  return 0;
     }

     fprintf(stderr, "incomplete read from joystick %ld", bytes);
     return -1;
     
}



void debug_print_joystick_event(const struct js_event * const event)
{
     fprintf(stderr, "Event: time %8u / value %8hd / type %8u / number %X\n",
	     event->time, event->value, event->type, event->number);
}
