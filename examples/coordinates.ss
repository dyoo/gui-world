#lang scheme/base

(define-struct dim (x-min 
                    y-min 
                    x-max
                    y-max
                    canvas-width
                    canvas-height
                    y-inverted?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Translation between coordinate systems:

;; canvas-x->coordinate-x: world number -> number
(define (canvas-x->coordinate-x a-world a-canvas-x)
  (round (+ (dim-x-min a-world) 
            (* (/ a-canvas-x (dim-canvas-width a-world))
               (- (dim-x-max a-world) (dim-x-min a-world))))))


;; coordinate-x->canvas-x: world number -> number
(define (coordinate-x->canvas-x a-world a-coordinate-x)
  (* (- a-coordinate-x (dim-x-min a-world))
     (/ (dim-canvas-height a-world)
        (- (dim-x-max a-world) (dim-x-min a-world)))))


;; canvas-y->coordinate-y: world number -> number
(define (canvas-y->coordinate-y a-world a-canvas-y)
  (round
   (+ (dim-y-min a-world) 
      (* (/ (- (dim-canvas-height a-world) a-canvas-y)
            (dim-canvas-height a-world))
         (- (dim-y-max a-world) (dim-y-min a-world))))))


;; coordinate-y->canvas-y: world number -> number
(define (coordinate-y->canvas-y a-world a-coordinate-y)
  (+ (dim-canvas-height a-world)
     (/ (* (- (dim-y-min a-world) a-coordinate-y)
           (dim-canvas-height a-world))
        (- (dim-y-max a-world) (dim-y-min a-world)))))