#lang scheme

(require "gui.ss"
         htdp/world)

;; Airline reservation

;; The world consists of a
(define-struct world (title name arrival departure submitted?))
;; where title is a string, name is a string, arrival and departure are strings within
;; TIMES, and submitted? is a boolean.



(define TIMES (list "2:00pm" 
                    "3:00pm" 
                    "4:00pm" 
                    "5:00pm" 
                    "6:00pm" 
                    "7:00pm")) 