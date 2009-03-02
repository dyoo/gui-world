#lang scheme
(require (prefix-in plot: plot)
         "../gui-world.ss")



;; The world consists of the name of a function and the body
(define-struct world (name args body plot dirty?) #:transparent)
(define-updaters world)

(define initial-world 
  (make-world "identity" (list "x") 
              "???"
              (empty-scene 500 500)
              #t))


;; on-replot-button-pressed: world -> world
(define (on-replot-button-pressed a-world)
  (let ([a-plot (plot:plot (plot:line (lambda (x) x)))])
    (update-world-dirty? 
     (update-world-plot a-world 
                        (place-image (put-pinhole a-plot 0 0)
                                     0
                                     0
                                     (empty-scene (image-width a-plot)
                                                  (image-height a-plot))))
     #f)))


;; on-text-field-change: world string -> world
(define (on-text-field-change a-world new-text)
  (update-world-dirty? 
   (update-world-body a-world new-text)
   #t))


;; on-canvas-redraw: world -> scene
(define (on-canvas-redraw a-world)
  (cond
    [(world-dirty? a-world)
     (let* ([out-of-sync-text (text "Out of sync" 10 "black")])
       (place-image out-of-sync-text
                    0 0
                    (place-image (nw:rectangle (image-width out-of-sync-text)
                                               (image-height out-of-sync-text)
                                               "solid"
                                               "yellow")
                                 0
                                 0
                                 (world-plot a-world))))]
    [else
     (world-plot a-world)]))


(define view
  (col
   (canvas on-canvas-redraw)
   (button "Replot" on-replot-button-pressed)
   (text-field world-body on-text-field-change)))


(big-bang initial-world view)