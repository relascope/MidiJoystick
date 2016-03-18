(c-declare "
#include \"glue.c\"
#include \"joystick.h\"
#include <stdio.h>
/*
int _send_midi(unsigned char* msg, size_t size) {
printf(\"msg: %x\\\n\", msg[0]);
if (jack_ringbuffer_write_space(jrb) < sizeof(size)) {
printf(\"no space in ringbuffer\\\n\");
return -1;
}
if (jack_ringbuffer_write(jrb, (void*)msg, size) != size) {
printf(\"cannot write full msg\\\n\");
return -1;
}
printf(\"all good\\\n\");
return 0;
}
*/

int fd_joy;

struct js_event input_event;


"
)

(thread-quantum-set! (current-thread) +inf.0) 

(c-define-type jack-ringbuffer-t "jack_ringbuffer_t")
(c-define-type jack-ringbuffer-t* (pointer jack-ringbuffer-t))

(c-define-type js-event (struct "js_event"))
(c-define-type js-event* (pointer js-event))
(define js-event-value (c-lambda () int16 "___return(input_event.value);"))
(define js-event-type (c-lambda () unsigned-int8 "___return(input_event.type);"))
(define js-event-number (c-lambda () unsigned-int8 "___return(input_event.number);"))
(define get-js-event (c-lambda (int) int "int res = get_joystick_event(___arg1, &input_event); ___return(res);"))
(define debug-print-js-event (c-lambda () void "debug_print_joystick_event(&input_event);"))
(define (print-js-event)
  (let ((value (js-event-value))
	(type (js-event-type))
	(number (js-event-number)))
    (print "event val: " value " type: " type " number: " number "\n")))





(define setup-jack (c-lambda () int "_setup_jack"))

;(define send-midi (c-lambda (scheme-object size_t) int "uint8_t *msg =  ___CAST(___U8*,___BODY_AS(___arg1,___tSUBTYPED)); int res = _send_midi(msg, ___arg2); puts(\"called c func\");  ___return(res);"))
(define send-midi (c-lambda (scheme-object size_t) int "int res = SEND_MIDI(___arg1, ___arg2);  ___return(res);"))
;(define send-midi (c-lambda ((pointer unsigned-int8) size_t) int "_send_midi" ))


(define open-joystick (c-lambda (nonnull-char-string) int "open_joystick" ))
(define close-joystick (c-lambda (int) int "close_joystick"))

(define (ev-id t i)
  (bitwise-and
   (bitwise-ior (bitwise-and (arithmetic-shift t 8) #xFF00) (bitwise-and i #xFF))
   #xFFFF))

(define *AXIS-REAL-MIN* -32767)
(define *AXIS-REAL-MAX* 32767)
(define *AXIS-POS-MAX* #xFFFF)
(define *AXIS-POS-MIN* #x0000)
(define *MIDI-VAL-MAX* #x7F)
(define *MIDI-VAL-MIN* #x00)

(define (js-val-to-midi val)
  (define (map-to-range v v-min v-max out-min out-max)
    (let ((slope (* 1.0 (/ (- out-max out-min) (- v-max v-min)))))
      (inexact->exact (round (+ out-min (* slope (- v  v-min)))))))
  (let ((pos-val (map-to-range val *AXIS-REAL-MIN* *AXIS-REAL-MAX* *AXIS-POS-MIN* *AXIS-POS-MAX*)))
    (bitwise-and *MIDI-VAL-MAX*  (map-to-range pos-val *AXIS-POS-MIN* *AXIS-POS-MAX* *MIDI-VAL-MIN* *MIDI-VAL-MAX*))))


(let ((args command-line))
  (let ((fd_joy (open-joystick "/dev/input/js0")))
    (setup-jack)

    (let loop ()
      (let ((ev-res (get-js-event fd_joy)))
	(if (eq? 0 ev-res) 
	    (if (and (eq? (js-event-type) #x02) (eq? (js-event-number) #x05))
		(begin 
		  (print-js-event)
		  (print "midi val: " (js-val-to-midi (js-event-value)) "\n\n")
		  (send-midi `(#xE0 ,(js-val-to-midi (js-event-value))) 2)
		  )
		(print-js-event))
	    (display "failed to get event\n")
	    )
	)
      (loop))
    (close-joystick)
    )
  )
