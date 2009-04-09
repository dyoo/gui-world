#lang scheme

;; Very simple program: a message on the pasteboard starts descending toward the bottom left,
;; based on changing the world.
;;
;; The style defines how the object will look.

(require "../gui-world.ss")

;; m is a message
(define m (message "hello world"))

;; The world is a number.
(define initial-world 0)

;; The view defines a message that will be positioned according to the css.
;; The css-f is a function from world to css.
(define view 
  (pasteboard (list m)
              
              #:css-f (lambda (world css)
                        (let* ([css (css-update css m 'top (number->string world))]
                               [css (css-update css m 'left (number->string world))])
                          css))))

(gui-big-bang initial-world view
          (on-tick 1/2 (lambda (world) (+ world 5))))