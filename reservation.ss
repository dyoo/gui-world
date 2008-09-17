#lang scheme

(require "gui.ss"
         htdp/world)

(define ... 'TODO)

;; Airline reservation

;; The world consists of a
(define-struct world (title name arrival departure submitted? error-message))
;; where title is a string, name is a string, arrival and departure are strings within
;; TIMES, and submitted? is a boolean.  Error-message is either #f or a string.
(define-updaters world)

(define initial-world (make-world "Unknown"
                                  ""
                                  "Unknown"
                                  "Unknown"
                                  #f
                                  #f))



(define TIMES (list "Unknown"
                    "2:00pm" 
                    "3:00pm" 
                    "4:00pm" 
                    "5:00pm" 
                    "6:00pm" 
                    "7:00pm"))


;; incomplete-submission?: world -> boolean
;; Returns true if the world submission hasn't all been filled out yet
(define (incomplete-submission? a-world)
  (or (string=? (world-title a-world) "Unknown")
      (string=? (world-name a-world) "")
      (string=? (world-arrival a-world) "Unknown")
      (string=? (world-departure a-world) "Unknown")))



;; on-submit: world -> world
;; When the user tries to submit, we see if the form is complete.  If it isn't,
;; show an error message on the next scene rendering.
(define (on-submit a-world)
  (cond [(incomplete-submission? a-world)
         (update-world-error-message a-world "Submission isn't complete.")]
        [else
         (update-world-submitted? a-world #t)]))


;; on-update-title: world string -> world
(define (on-update-title a-world a-val)
  (update-world-error-message (update-world-title a-world a-val)
                              #f))


;; on-update-name: world string -> world
(define (on-update-name a-world a-val)
  (update-world-error-message (update-world-name a-world a-val)
                              #f))


;; on-update-departure: world string -> world
(define (on-update-departure a-world a-val)
  (update-world-error-message (update-world-departure a-world a-val)
                              #f))


;; on-update-arrival: world string -> world
(define (on-update-arrival a-world a-val)
  (update-world-error-message (update-world-arrival a-world a-val)
                              #f))



(define (world->form-scene a-world)
  (cond
    [(world-submitted? a-world)
     (make-form "Your reservation "
                (make-row (world-title a-world)
                          (world-name a-world)
                          (world-arrival a-world)
                          (world-departure a-world))
                "has been submitted")]
    [else 
     (make-form
      (maybe-make-error (world-error-message a-world))
      (make-row "Title" (make-drop-down (world-title a-world) 
                                        (list "Unknown"
                                              "Mr."
                                              "Ms."
                                              "Mrs.")
                                        update-world-title))
      (make-row "Name" (make-text-field (world-name a-world)
                                        update-world-name))
 
      (make-row "Departure" (make-drop-down (world-departure a-world)
                                            TIMES
                                            update-world-departure))
      
      (make-row "Arrival" (make-drop-down (world-arrival a-world)
                                          TIMES
                                          update-world-arrival))
      
      (make-button "Submit" on-submit))]))