#lang scheme
;; Test for reentrancy.  Button press will call big-bang recursively, and then come back to the original.
(require "../gui-world.ss")

(define-struct world-1 (finished?) #:transparent)
(define-struct world-2 (finished?) #:transparent)

(define INITIAL-WORLD-1 (make-world-1 false))
(define INITIAL-WORLD-2 (make-world-2 false))

(define (finish-world-1 w)
  (make-world-1 true))

(define (finish-world-2 w)
  (make-world-2 true))
        

(define (visit-view-2 w)
  (restore-world-1 (gui-big-bang INITIAL-WORLD-2 view-2 
                                 (stop-when (lambda (w)
                                              (printf "stopping? ~s~n" w) 
                                              (world-2-finished? w))))
                   w))

(define (restore-world-1 w-2 w-1)
  w-1)
  
(define view-1
  (row (button "Go to view 2" visit-view-2)
       (button "Finish" finish-world-1)))

(define view-2
  (row (button "Ok" finish-world-2)))


(define last-world
  (gui-big-bang INITIAL-WORLD-1 view-1
                (stop-when world-1-finished?)))