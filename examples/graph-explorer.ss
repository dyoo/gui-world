#lang scheme
(require (prefix-in plot: plot)
         "../gui-world.ss")

;; The world consists of the name of a function and the body
(define-struct world (name args body))
(define-updaters world)

(define initial-world 
  (make-world "identity" (list "x") 
              "x"))


(define (draw-function-canvas a-world)
  (let ([a-plot (plot:plot (plot:line (lambda (x) x)))])
    (place-image (put-pinhole a-plot 0 0)
                 0
                 0
                 (empty-scene (image-width a-plot)
                              (image-height a-plot)))))


;; on-replot-button-pressed: world -> world
(define (on-replot-button-pressed a-world)
  a-world)


(define view
  (col
   (canvas draw-function-canvas)
   (button "Replot" on-replot-button-pressed)
   (text-field world-body update-world-body)))



(big-bang initial-world view)