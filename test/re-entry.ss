#lang scheme

;; Test for reentrancy.  Button press will call big-bang recursively, and then come back to the original.
(require "../gui-world.ss")

;; We use two worlds here to make sure we're getting the types of the world and "subworld" correct.
(define-struct world-1 (n finished?) #:transparent)
(define-struct world-2 (k finished?) #:transparent)

(define-updaters world-1)
(define-updaters world-2)


(define INITIAL-WORLD-1 (make-world-1 0 false))
(define INITIAL-WORLD-2 (make-world-2 0 false))


;; finish-world-1: world-1 -> world-1
;; Marks world-1 as finished.
(define (finish-world-1 w)
  (update-world-1-finished? w true))


;; finish-world-2: world-2 -> world-2
;; Marks world-2 as finished.
(define (finish-world-2 w)
  (update-world-2-finished? w true))
        

;; visit-view-2: world-1 -> world-1
;; Call gui-big-bang re-entrantly, do some processing, and return
;; a new world-1.
(define (visit-view-2 w)
  (let ([new-world-2
         (gui-big-bang INITIAL-WORLD-2 view-2 
                       (stop-when world-2-finished?))])
    (update-world-1-n w
                      (world-2-k new-world-2))))


  
(define view-1
  (row (message (lambda (w) 
                  (string-append "The world contains: " (number->string (world-1-n w)))))
       (button "Go to view 2" visit-view-2)
       (button "Finish" finish-world-1)))

(define view-2
  (row 
   ;; show the current value of world-2's k value, and pressing will increment it.
   (button world-2-k 
           (lambda (w) (update-world-2-k w (add1 (world-2-k w)))))
   (button "Go back" finish-world-2)))


(define last-world
  (gui-big-bang INITIAL-WORLD-1 view-1
                (stop-when world-1-finished?)))