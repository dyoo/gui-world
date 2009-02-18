;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname graph-function) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Graph function: representing partial math functions as graphs.

(require "../../gui-world.ss")


;;  A world consists of the bounds on the graph.
(define-struct world (x-min    ;; number
                      x-max    ;; number
                      y-min    ;; number
                      y-max    ;; number
                      points   ;; (listof posn)
                      ))
(define-updaters world)

(define initial-world (make-world -10 10 -10 10 empty))

(define CANVAS-WIDTH 300)
(define CANVAS-HEIGHT 300)



;; render-canvas: world -> scene
;; Renders the canvas containing all the points.
(define (render-canvas a-world)
  (render-canvas-points a-world 
                        (world-points a-world)
                        (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)))



;; render-canvas-points: world (listof posn) scene -> scene
(define (render-canvas-points a-world points a-scene)
  (cond
    [(empty? points)
     a-scene]
    [else
     (render-canvas-points a-world 
                           (rest points)
                           (place-image (circle 5 "solid" "black")
                                        (coordinate-x->canvas-x (posn-x (first points)))
                                        (coordinate-y->canvas-y (posn-y (first points)))
                                        a-scene))]))


;; place-point: world number number -> world
(define (place-point a-world x y)
  (update-world-points a-world
                       (cons (make-posn (canvas-x->coordinate-x a-world x)
                                        (canvas-y->coordinate-y a-world y))
                             (world-points a-world))))


(define (canvas-x->coordinate-x a-world a-canvas-x)
  ...)

(define (coordinate-x->canvas-x a-world a-coordinate-x)
  ...)

(define (canvas-y->coordinate-y a-world a-canvas-y)
  ...)

(define (coordinate-y->canvas-y a-world a-coordinate-y)
  ...)


  

;; The view includes the canvas.  Clicks on the canvas add new points.
(define view
  (col (canvas/callback world->scene place-point)))