#lang scheme

;; A message on the pasteboard descends toward the bottom right.
;; based on changing the world.
;;
;; The style defines how the object will look.

(require "../../private/gui-world.ss")

;; m is a message
(define m (message "hello world"))

;; The world is a number.
(define initial-world 0)

;; The view defines a message that will be positioned according to the css.
;; The css-f is a function from world to css.
(define view 
  (pasteboard (list m)
              
              #:css-f (lambda (world css)
                        (let* ([css (css-update css m 'top world)]
                               [css (css-update css m 'left world)])
                          css))))

(big-bang initial-world view
          (on-tick 1/2 (lambda (world) (+ world 5))))