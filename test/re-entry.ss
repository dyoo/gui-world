;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname re-entry) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Test for reentrancy.  Button press will call big-bang recursively, and then come back to the original.
(require "../gui-world.ss")

;; We use two worlds here to make sure we're getting the types of the world and "subworld" correct.
(define-struct world-1 (n finished?))
(define-struct world-2 (k finished?))

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


;; world-2-increment: world-2 -> world-2
;; Adds one to the k value of w2.
(define (world-2-increment w2)
  (update-world-2-k w2 (add1 (world-2-k w2))))



;; visit-view-2: world-1 -> world-1
;; Call gui-big-bang re-entrantly, do some processing, and return
;; a new world-1.
(define (visit-view-2 w)
  (revise w (gui-big-bang INITIAL-WORLD-2 view-2 
                       (stop-when world-2-finished?))))


;; revise: world-1 world-2 -> world-1
;; Revises the content of w1 with new information from w2.
(define (revise w1 w2)
  (update-world-1-n w1
                    (world-2-k w2)))


;; description: world-1 -> string
;; Produces a string description.
(define (description w)
  (string-append "The world contains: " (number->string (world-1-n w))))
  
(define view-1
  (row (message description)
       (button "Go to view 2" visit-view-2)
       (button "Finish" finish-world-1)))

(define view-2
  (row 
   ;; show the current value of world-2's k value, and pressing will increment it.
   (button world-2-k world-2-increment)

   (button "Go back" finish-world-2)))


(define last-world
  (gui-big-bang INITIAL-WORLD-1 view-1
                (stop-when world-1-finished?)))