#lang scheme
(require plot
         "../gui-world.ss")

;; The world consists of the name of a function and the body
(define world (name body))


(define (draw-function-canvas a-world)
  (plot (line (lambda (x) x))))

(define (function-body a-world)
  "x")

(define (update-function-body a-world a-new-body)
  a-world)


(define view
  (row
   (canvas draw-function-canvas)
   (text function-body update-function-body)))