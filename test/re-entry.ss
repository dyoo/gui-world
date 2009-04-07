;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname re-entry) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Test for reentrancy.  Button press will call big-bang recursively, and then come back to the original.
(require "../gui-world.ss")

(define-struct world-1 (finished?))
(define-struct world-2 (finished?))



(define (finish-world-1 w)
  (make-world-1 true))

(define (finish-world-2 w)
  (make-world-2 true))
        
(define (visit-view-2 w)
  (gui-big-bang false view-2 
                (stop-when world-2-finished?)))

(define view-1
  (row (button "Go to view 2" visit-view-2)
       (button "Finish" finish-world-1)))

(define view-2
  (row (button "Ok" finish-world-2)))


(define last-world
  (gui-big-bang (make-world-1 false) view-1
                (stop-when world-1-finished?)))