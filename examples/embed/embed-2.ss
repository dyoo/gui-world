#lang scheme

;; Pressing a button increments the world and causes the column to descend.
;;
(require "../../private/gui-world.ss")

;; A world is a number.
(define INITIAL-WORLD 0)


(define m (message (lambda (w) (format "I see: ~a~n" w))))

(define b 
  (button "Press me"
          (lambda (w)
            (add1 w))))

(define c (col m b))


(define VIEW
  (pasteboard (list c)
              
   #:css-f (lambda (world css)
             ;; Position the column c down at (* world 5).
             (css-update css c 'top (* world 5)))))


(big-bang INITIAL-WORLD VIEW
          #:css (css-update (css-update (make-css) c 'top 0)
                            c 'left 100))