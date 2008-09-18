#lang scheme

(require "gui-world.ss")

;; Airline reservation.  Program disables the submit bug until we're happy, so the
;; submit error can't happen.

;; The world consists of a
(define-struct world (title name arrival departure submitted?))
;; where title is a string, name is a string, arrival and departure are strings within
;; TIMES, and submitted? is a boolean.
(define-updaters world)

(define initial-world (make-world "Unknown"
                                  ""
                                  "Unknown"
                                  "Unknown"
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

(define (complete-submission? a-world)
  (not (incomplete-submission? a-world)))


;; on-submit: world -> world
(define (on-submit a-world)
  (update-world-submitted? a-world #t))



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
      
      (make-button "Submit" on-submit (complete-submission? a-world)))]))

(big-bang 300 300 initial-world)
(on-redraw world->form-scene)