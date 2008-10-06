#lang scheme

(require "../gui-world.ss")

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

(define TITLES (list "Unknown"
                     "Mr."
                     "Ms."
                     "Mrs."))

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


;; completed-unsubmitted-form?: world -> boolean
(define (completed-unsubmitted-form? a-world)
  (and (not (incomplete-submission? a-world))
       (not-submitted? a-world)))

;; on-submit: world -> world
;; Submits the form, not allowing any further changes.
(define (on-submit a-world)
  (update-world-submitted? a-world #t))


;; not-submitted?: world -> boolean
;; Returns true if the world isn't yet submitted.
(define (not-submitted? a-world)
  (not (world-submitted? a-world)))


;; world-status-message: world -> string
;; Consumes the world, and produces
(define (world-status-message a-world)
  (cond 
    [(world-submitted? a-world)
     (string-append "Your reservation "
                    (world-title a-world)
                    " "
                    (world-name a-world)
                    " "
                    (world-arrival a-world)
                    " "
                    (world-departure a-world)
                    " has been submitted")]
    [else
     "Your reservation is not complete yet."]))
   

(define a-gui
  (col 
   (row "Title" (drop-down world-title TITLES update-world-title not-submitted?))
   (row "Name" (text-field world-name update-world-name not-submitted?))
   (row "Departure" (drop-down world-departure TIMES update-world-departure not-submitted?))
   (row "Arrival" (drop-down world-arrival TIMES update-world-arrival not-submitted?))
   (button "Submit" on-submit completed-unsubmitted-form?)
   (message world-status-message)))


(big-bang initial-world a-gui)
