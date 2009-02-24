#lang scheme/base
(require scheme/math
         scheme/contract
         htdp/world)



;; draw-arrow: number number number number color scene -> scene
;; Draws an arrow from (x1, y1) to (x2, y2)
(define (draw-arrow x1 y1 x2 y2 color a-scene)
  (let ([L (line (- x2 x1)
                 (- y2 y1)
                 color)])
    (cond
      [(and (= x1 x2)
            (= y1 y2))
       (place-image (arrowhead color 0)
                    x1
                    y1
                    a-scene)]
      [else
       (place-image (arrowhead color (angle x1 y1 x2 y2))
                    x2 
                    y2
                    (place-image L x1 y1 a-scene))])))



;; arrowhead: color number -> image
;; Produces an arrowhead of the given color and orientation.
(define (arrowhead color rad)
  (let ([sides 3]
        [radius 7])
    (regular-polygon sides radius "solid" color rad)))
  

;; angle: number number number number -> number
;; Computes the angle of rotation needed for the arrowhead, assuming
;; the arrowhead is originally oriented to the right.
(define (angle x1 y1 x2 y2)
  (cond
    ;; Quadrant 1
    [(and (positive? (- x2 x1))
          (positive? (- y2 y1)))
     (+ (atan (/ (+ (- y2 y1) 0.0)
                 (+ (- x2 x1) 0.0))))]
    ;; Quadrant 2
    [(and (negative? (- x2 x1))
          (positive? (- y2 y1)))
     (+ (atan (/ (+ (- x1 x2) 0.0)
                 (+ (- y2 y1) 0.0)))
        (* pi 1/2))]
    ;; Quadrant 3
    [(and (negative? (- x2 x1))
          (negative? (- y2 y1)))
     (+ (atan (/ (+ (- y1 y2) 0.0)
                 (+ (- x1 x2) 0.0)))
        pi)]
    ;; Quadrant 4
    [(and (positive? (- x2 x1))
          (negative? (- y2 y1)))
     (+ (atan (/ (+ (- x2 x1) 0.0)
                 (+ (- y1 y2) 0.0)))
        (* pi 3/2))]
    [else
     ;; on axis
     (cond [(and (< x1 x2)
                 (= y1 y2))
            0]
           [(and (= x1 x2)
                 (< y1 y2))
            (* pi 1/2)]
           [(and (> x1 x2)
                 (= y1 y2))
            (* pi 1)]
           [(and (= x1 x2)
                 (> y1 y2))
            (* pi 3/2)]
           [else
            (printf "ugh: ~s~n" (list x1 y1 x2 y2))])]))



(define (scene? x)
  ;; fixme!
  #t)

(provide/contract [draw-arrow
                   (number?
                    number?
                    number?
                    number? 
                    (or/c string? color?)
                    scene? 
                    . -> . scene?)])
