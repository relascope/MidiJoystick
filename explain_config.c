#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include "config.h"
#include "joystick.h"


#define DEVICE_PATH "/dev/input/js0"
#define CONFIG_PATH "./input.conf"
int main(void)
{

	  
     struct input_conf* max_mapping = NULL;

     int rs;
     int js_fd;
     int n_axes, n_btns, n_inputs;
     
     js_fd = open_joystick(DEVICE_PATH);

     if ((n_axes = get_number_of_axes(js_fd)) == -1)
	  n_axes = 0;
     if ((n_btns = get_number_of_buttons(js_fd)) == -1)
	  n_btns = 0;


     n_inputs = n_btns + n_axes;
     printf("Found %d axes and %d buttons\n", n_axes, n_btns);
     
     if (n_inputs < 1) {
	  fprintf(stderr, "joystick seems to have no axes nor buttons\n");
	  return EXIT_FAILURE;
     }

     errno = 0;
     if (!(max_mapping = calloc(n_inputs, sizeof(*max_mapping)))) {
	  perror("failed allocating memory\n");
	  return EXIT_FAILURE;
     }


     rs = read_config_file(CONFIG_PATH, max_mapping, n_inputs);
     if(rs < 1) {
	  free(max_mapping);
	  return rs;
     }
     struct input_conf mapping[sizeof(struct input_conf) * rs];
     memcpy(mapping, max_mapping, sizeof(struct input_conf) * rs);

     free(max_mapping);
     explain_configuration(mapping, rs);

     return 0;
}
	  
