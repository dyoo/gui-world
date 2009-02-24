#lang scheme/base
(require scheme/math
         scheme/contract
         htdp/world)

(define (deg->rad a-deg)
  (/ (* a-deg pi) 180))

(define (rad->deg a-rad)
  (/ (* a-rad 180) pi))

(define ... 'todo)


(define (arrowhead color rad)
  (let ([sides 3]
        [radius 7])
    (regular-polygon sides 
                   radius 
                   "solid"
                   color
                   ;; Note: if x2 == x1, then
                   ;; the inexact coersions allow us
                   ;; to compute the right degree.
                   rad)))
  

;; angle: number number number number -> number
(define (angle x1 y1 x2 y2)
  (cond
    ;; Quadrant 1
    [(and (positive? (- x2 x1))
          (positive? (- y2 y1)))
     (printf "quad 1~n")
     (+ (atan (/ (+ (- y2 y1) 0.0)
                 (+ (- x2 x1) 0.0))))]
    ;; Quadrant 2
    [(and (negative? (- x2 x1))
          (positive? (- y2 y1)))
     (printf "quad 2~n")
     (+ (atan (/ (+ (- x1 x2) 0.0)
                 (+ (- y2 y1) 0.0)))
        (* pi 1/2))]
    ;; Quadrant 3
    [(and (negative? (- x2 x1))
          (negative? (- y2 y1)))
     (printf "quad 3~n")
     (+ (atan (/ (+ (- y1 y2) 0.0)
                 (+ (- x1 x2) 0.0)))
        pi)]
    ;; Quadrant 4
    [(and (positive? (- x2 x1))
          (negative? (- y2 y1)))
     (printf "quad 4~n")
     (+ (atan (/ (+ (- x2 x1) 0.0)
                 (+ (- y1 y2) 0.0)))
        (* pi 3/2))]
    [else
     (+ (atan (/ (abs (+ (- y2 y1) 0.0))
                 (abs (+ (- x2 x1) 0.0)))))]))


                        
(define (draw-arrow x1 y1 x2 y2 color a-scene)
  (let ([L (line (- x2 x1)
                 (- y2 y1)
                 color)])
    (cond
      [(and (= x1 x2)
            (= y1 y2))
       a-scene]
      [else
       (place-image (arrowhead color (angle x1 y1 x2 y2))
                    x2 
                    y2
                    (place-image L x1 y1 a-scene))])))


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
