#lang scheme

(require "../gui-world.ss")

(define m (message "hello world"))

(gui-big-bang 42
          (pasteboard (list m)
                      #:css-f (lambda (world css)
                                (css-update css m 'top (number->string world))))
          (on-tick 1/2 (lambda (world) 
                       (+ world 5))))