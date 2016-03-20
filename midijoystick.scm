(c-declare
"
#include \"glue.c\"
#include \"joystick.h\"
struct js_event input_event;
")


;; Global Constants
(define *JS-EVENT-AXIS* #x02)
(define *JS-EVENT-BUTTON* #x01)
(define *BUTTON-PRESSED* #x01)
(define *BUTTON-RELEASED* #x00)



;; Misc Helper Functions

;; rotate list left: (rotate-left '(1 2 4)) -> '(4 1 2)
(define (rotate-left lst)
  `(,@(cdr lst) ,@(cons (car lst) '())))


;; map val from range [val-min,val-max] into output range [out-min,out-max]
(define (map-to-range val val-min val-max out-min out-max)
  (let ((slope (* 1.0 (/ (- out-max out-min) (- val-max val-min)))))
    (inexact->exact (round (+ out-min (* slope (- val val-min)))))))


;; returns a hash (ev-id #x40 #30) -> #x4030
(define (ev-id t i)
  (bitwise-and
   (bitwise-ior (bitwise-and (arithmetic-shift t 8) #xFF00) (bitwise-and i #xFF))
   #xFFFF))



;; C Interface

;; Functions to interact with static allocated struct js_event
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


;; Functions to interact with JackAudioServer
(define setup-jack (c-lambda () int "_setup_jack"))
(define send-midi (c-lambda (scheme-object size_t) int "int res = SEND_MIDI(___arg1, ___arg2);  ___return(res);"))


;; Functions to interact with joystick device file
(define open-joystick (c-lambda (nonnull-char-string) int "open_joystick" ))
(define close-joystick (c-lambda (int) int "close_joystick"))



;; Configfile Parsing

;; data structure to hold a commandbinding
(define-structure midi cmd ch param func)

;; returns a table(k,v) where: k := (ev-id t i); v = '(<instance[s] of midi> ... )
(define (parse-config path)


  ;; returns a (lambda (joystick-input-val) (...)) function based on MIDI Command found in midi-conf (which is a midi structure)
  (define (midi-func-dispatch midi-conf)

    ;; constant values needed by this function
    (define *AXIS-REAL-MIN* -32767)
    (define *AXIS-REAL-MAX* 32767)
    (define *AXIS-POS-MAX* #xFFFF)
    (define *AXIS-POS-MIN* #x0000)
    (define *MIDI-VAL-MAX* #x7F)
    (define *MIDI-VAL-MIN* #x00)
    (define *MIDI-Pitch-Bend-MAX* #x3FFF)

    (define *MIDI-CMD-Note-Off* #x80)
    (define *MIDI-CMD-Note-On* #x90)
    (define *MIDI-CMD-Continus-Controller* #xB0)
    (define *MIDI-CMD-Patch-Change* #xC0)
    (define *MIDI-CMD-Channel-Pressure* #xD0)
    (define *MIDI-CMD-Pitch-Bend* #xE0)
    (define *MIDI-CMD-SYSEX* #xF0)
    
    ;; maps +/- joystick input value into positive spectrum
    (define (js-val-to-pos-js-val val)
      (map-to-range val *AXIS-REAL-MIN* *AXIS-REAL-MAX* *AXIS-POS-MIN* *AXIS-POS-MAX*))

    ;; maps a joystick input first to positive spectrum, then into midi spectrum
    (define (js-val-to-midi val)  
      (let ((pos-val (js-val-to-pos-js-val val)))
	(bitwise-and *MIDI-VAL-MAX*  (map-to-range pos-val *AXIS-POS-MIN* *AXIS-POS-MAX* *MIDI-VAL-MIN* *MIDI-VAL-MAX*))))


    ;; lambda function creators
    
    (define (build-midi-cmd-byte midi-conf)
      (cond ((equal? (midi-cmd midi-conf) #xF0) #xF0)
	    (else (bitwise-ior (midi-cmd midi-conf) (midi-ch midi-conf)))))

    (define (build-msg-input-last midi-conf)
      (lambda (input-val) `(,(build-midi-cmd-byte midi-conf) ,@(midi-param midi-conf) ,(js-val-to-midi input-val))))

    (define (build-msg-input-first midi-conf)
      (lambda (input-val) `(,(build-midi-cmd-byte midi-conf) ,(js-val-to-midi input-val) ,@(midi-param midi-conf))))

    (define (build-msg-no-input midi-conf)
      (lambda (input-val) `(,(build-midi-cmd-byte midi-conf) ,@(midi-param midi-conf))))

    (define (build-msg-no-param midi-conf)
      (lambda (input-val) `(,(build-midi-cmd-byte midi-conf) ,(js-val-to-midi input-val))))

    (define (build-msg-note-on/off midi-conf)
      (lambda (input-val) `(,(build-midi-cmd-byte midi-conf) ,(+ (if (null? (midi-param midi-conf)) 0 (car (midi-param midi-conf))) (js-val-to-midi input-val)) #x7F)))

    (define (build-msg-pitch-bend midi-conf)
      (let ((split-input-val (lambda (input-val) (cons (bitwise-and input-val *MIDI-VAL-MAX*) (cons (bitwise-and (arithmetic-shift input-val -7) *MIDI-VAL-MAX*) '())))))
	(lambda (input-val) `(,(build-midi-cmd-byte midi-conf) ,@(split-input-val (map-to-range
										   (js-val-to-pos-js-val input-val)
										   *AXIS-POS-MIN* *AXIS-POS-MAX* *MIDI-VAL-MIN* *MIDI-Pitch-Bend-MAX*))))))
    ;; dispatcher
    
    (let ((cmd (midi-cmd midi-conf)))
      (cond ((equal? cmd *MIDI-CMD-Continus-Controller*) (build-msg-input-last midi-conf))
	    ((equal? cmd *MIDI-CMD-Patch-Change*) (build-msg-no-input midi-conf))
	    ((equal? cmd *MIDI-CMD-Channel-Pressure*) (build-msg-no-param midi-conf))
	    ((equal? cmd *MIDI-CMD-Pitch-Bend*) (build-msg-pitch-bend midi-conf))
	    ((equal? cmd *MIDI-CMD-SYSEX*) (build-msg-no-input midi-conf))
	    ;; ((or (equal? cmd *MIDI-CMD-Note-Off*) (equal? cmd *MIDI-CMD-Note-On*)) (build-msg-note-on/off midi-conf))
	    (else (lambda (input-val) '() )))))



  ;; short helper to create an instance of midi structure based on commandbinding
  (define (create-midi-struct command-binding)
    (let* ((cmd-ch (car command-binding))
	   (midi (make-midi (car cmd-ch)
			    (if (pair? (cdr cmd-ch)) (cadr cmd-ch))
			    (cdr command-binding)
			    (lambda (input-val) '()))))
      (midi-func-set! midi (midi-func-dispatch midi))
      midi))

  ;; traverse a list of commandbindings and returns a list of midi instances
  (define (create-list-of-midi-structs command-bindings)
    (if (null? command-bindings) command-bindings
	(cons (create-midi-struct (car command-bindings)) (create-list-of-midi-structs (cdr command-bindings)))))

  
  (let ((config-list (call-with-input-file path read))
	(config-table (make-table)))
    ;; creates a table entry [k:(ev-id t i), v:'(<instance[s] of midi>) based on an configuration entry
    (define (make-entry entry)
      (let* ((id (car entry))
	     (command-bindings (cdr entry))
	     (t (cond ((equal? (car id) 'a) *JS-EVENT-AXIS*)
		      ((equal? (car id) 'b) *JS-EVENT-BUTTON*)
		      (else #f)))
	     (i (cadr id)))
	(display command-bindings) (newline)
	(if (equal? t #f) (begin (display  "failed to parse line: ") (display entry) (newline)) 
	    (table-set! config-table (ev-id t i) (create-list-of-midi-structs command-bindings)))))

    ;; loop traversing config entries
    (let loop ((config-list config-list))
      (make-entry (car  config-list))
      (if (not (null? (cdr config-list)))
	  (loop (cdr config-list))))
    config-table))



(define (get-midi-msgs config-table)

  ; returns single element list of '(lambda (input-val) (...)), rotates stored midi instances in config-table left by one
  (define (build-button-command-list event-id)
    (let ((midi-lst (table-ref config-table event-id `(,(make-midi #x00 #x00 '() (lambda (x) '()))))))
      (table-set! config-table event-id (rotate-left midi-lst))
      (cons (midi-func (car midi-lst)) '())))

  ; returns '(lambda (input-val) (...) ...) retrieved from config table by event-id
  (define (build-axis-command-list event-id)
    (letrec ((build (lambda (lst) (if (null? lst) lst
				      (cons (midi-func (car lst))
					    (build (cdr lst)))))))
      (build (table-ref config-table event-id `(,(make-midi #x00 #x00 '() (lambda (x) '())))))))
  
  (let* ((event-type (js-event-type))
	 (event-id (ev-id event-type (js-event-number))))
    (cond ((equal? event-type *JS-EVENT-AXIS*) ; handle axis event
	   (build-axis-command-list event-id))
	  ((and (equal? event-type *JS-EVENT-BUTTON*)
		(equal? (js-event-value) *BUTTON-PRESSED*)) ; handle button presed event
	   (build-button-command-list event-id))
	  (else `(,(lambda (x) '()))))))


;; MAIN

(let* ((args (command-line))
       (js-file-flag (member "-j" args))
       (conf-file-flag (member "-c" args)))
  (display args) (newline)
  
  (let ((fd-joy (if (and js-file-flag (> (length js-file-flag) 1))
		    (open-joystick (cadr js-file-flag))
		    (open-joystick "/dev/input/js0")))
	(config (if (and conf-file-flag (> (length conf-file-flag) 1))
		    (parse-config (cadr conf-file-flag))
		    (parse-config "./input.conf"))))

    (setup-jack) 

    (let mainloop ()
      (let ((ev-res (get-js-event fd-joy)))
	(if (equal? 0 ev-res) ; test if we got a valid js_event
	    (let ((midi-list (get-midi-msgs config))
		  (event-value (js-event-value)))
	      (let sendloop ((midi-list midi-list))
		(let* ((msg  ((car midi-list) (if (and (equal? (js-event-type) *JS-EVENT-BUTTON*)
						       (equal? event-value *BUTTON-PRESSED*))
						  #x7F
						  event-value)))
		       (msg-length (length msg)))
		  (if (not (equal? msg-length 0))
		      (begin 
			(display msg) (newline)
			(send-midi msg (length msg)))))
		(if (not (null? (cdr midi-list)))
		    (sendloop (cdr midi-list)))))
	    (display "failed to get event\n")))
      (mainloop))

    (close-joystick fd-joy)))
