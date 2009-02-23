#lang scheme/base
(require scheme/math
         htdp/world)

(define (deg->rad a-deg)
  (/ (* a-deg pi) 180))

(define (rad->deg a-rad)
  (/ (* a-rad 180) pi))

(define ... 'todo)


(define (arrowhead rad)
  (let ([sides 3]
        [radius 7])
  (regular-polygon sides 
                   radius 
                   "solid"
                   "black"
                   ;; Note: if x2 == x1, then
                   ;; the inexact coersions allow us
                   ;; to compute the right degree.
                   rad)))
  

(define (draw-arrow x1 y1 x2 y2 a-scene)
  (let ([L (line (- x2 x1)
                 (- y2 y1)
                 "black")]
        [T 
         (arrowhead (atan (/ (+ (- y2 y1) 0.0)
                             (+ (- x2 x1) 0.0))))])
    (place-image T
                 x2 
                 y2
                 (place-image L x1 y1 a-scene))))
