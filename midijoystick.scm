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


(define *MIDI-CMD-Continus-Controller* #xB0)
(define *MIDI-CMD-Patch-Change* #xC0)
(define *MIDI-CMD-Channel-Pressure* #xD0)
(define *MIDI-CMD-Pitch-Bend* #xE0)

(define (midi-func-dispatch midi-conf)

  (define (build-midi-cmd-byte midi-conf)
    (cond ((equal? (midi-cmd midi-conf) #xF0) #xF0)
	  (else (bitwise-ior (midi-cmd midi-conf) (midi-ch midi-conf)))))

  (define (build-msg-input-last midi-conf)
    (lambda (input-val) `(,(build-midi-cmd-byte midi-conf) ,@(midi-param midi-conf) ,input-val)))

  (define (build-msg-input-first midi-conf)
    (lambda (input-val) `(,(build-midi-cmd-byte midi-conf) ,input-val ,@(midi-param midi-conf))))

  (define (build-msg-no-input midi-conf)
    (lambda (input-val) `(,(build-midi-cmd-byte midi-conf) ,@(midi-param midi-conf))))

  (define (build-msg-no-param midi-conf)
    (lambda (input-val) `(,(build-midi-cmd-byte midi-conf) ,input-val)))
  
  (let ((cmd (midi-cmd midi-conf)))
    (cond ((equal? cmd *MIDI-CMD-Continus-Controller*) (build-msg-input-last midi-conf))
	  ((equal? cmd *MIDI-CMD-Patch-Change*) (build-msg-no-input midi-conf))
	  ((equal? cmd *MIDI-CMD-Channel-Pressure*) (build-msg-no-param midi-conf))
	  ((equal? cmd *MIDI-CMD-Pitch-Bend*) (build-msg-no-param midi-conf))
	  (else (lambda (input-val) '() ))))
)

(define (ev-id t i)
  (bitwise-and
   (bitwise-ior (bitwise-and (arithmetic-shift t 8) #xFF00) (bitwise-and i #xFF))
   #xFFFF))

(define *JS-EVENT-AXIS* #x02)
(define *JS-EVENT-BUTTON* #x01)

(define-structure midi cmd ch param func)

(define (parse-config path)
  (let ((config-list (call-with-input-file path read))
	(config-table (make-table)))

    (define (make-entry entry)
      (let* ((id (car entry))
	     (actions (cadr entry))
	     (t (cond ((equal? (car id) 'a) *JS-EVENT-AXIS*)
		      ((equal? (car id) 'b) *JS-EVENT-BUTTON*)
		      (else #f)))
	     (i (cadr id)))
	(display actions) (newline)
	(if (equal? t #f) (begin (display  "failed to parse line: ") (display entry) (newline)) 
	    (table-set! config-table (ev-id t i)
			(let* ((cmd-ch (car actions))
			      (midi (make-midi (car cmd-ch)
					       (if (pair? (cdr cmd-ch)) (cadr cmd-ch) #x0)
					       (cdr actions)
					       (lambda (input-val) '()))))
			  (midi-func-set! midi (midi-func-dispatch midi))
			  midi)))))
    
	 (let loop ((config-list config-list))
	   (make-entry (car  config-list))
	   (if (not (null? (cdr config-list)))
	       (loop (cdr config-list))))
	 config-table))


(define (get-msg-by-ev-id config-table)
  (let ((event-id (ev-id (js-event-type) (js-event-number))))
    (midi-func (table-ref config-table event-id (make-midi #x00 #x00 '() (lambda (x) '()))))))



(define *AXIS-REAL-MIN* -32767)
(define *AXIS-REAL-MAX* 32767)
(define *AXIS-POS-MAX* #xFFFF)
(define *AXIS-POS-MIN* #x0000)
(define *MIDI-VAL-MAX* #x7F)
(define *MIDI-VAL-MIN* #x00)

(define (js-val-to-midi val)  
  (define (map-to-range val v-min v-max out-min out-max)
    (let ((slope (* 1.0 (/ (- out-max out-min) (- v-max v-min)))))
      (inexact->exact (round (+ out-min (* slope (- val v-min)))))))
  
  (let ((pos-val (map-to-range val *AXIS-REAL-MIN* *AXIS-REAL-MAX* *AXIS-POS-MIN* *AXIS-POS-MAX*)))
    (bitwise-and *MIDI-VAL-MAX*  (map-to-range pos-val *AXIS-POS-MIN* *AXIS-POS-MAX* *MIDI-VAL-MIN* *MIDI-VAL-MAX*))))



(let ((args command-line))
  (let ((fd-joy (open-joystick "/dev/input/js0"))
	(config (parse-config "./input.conf")))

    (setup-jack)

    (let loop ()
      (let ((ev-res (get-js-event fd-joy)))
	(if (equal? 0 ev-res)
	    (let ((msg ((get-msg-by-ev-id config) (js-val-to-midi (js-event-value)))))
		(send-midi msg (length msg)))
		    
	    
	    (display "failed to get event\n")
	    )
	)
      (loop))

    (close-joystick fd-joy)))
