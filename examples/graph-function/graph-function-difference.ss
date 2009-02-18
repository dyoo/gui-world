;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname graph-function-difference) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Graph function: representing partial math functions as graphs.

(require "../../gui-world.ss")

;; An io consists of an input and an output.
(define-struct io (input    ;; input
                   output   ;; posn
                   ))
(define-updaters io)

;;  A world consists of the bounds on the graph.
(define-struct world (x-min         ;; number
                      x-max         ;; number
                      y-min         ;; number
                      y-max         ;; number
                      ios           ;; (listof io)
                      current-input ;; input
                      ))
(define-updaters world)


(define CANVAS-WIDTH 300)
(define CANVAS-HEIGHT 300)

(define MIN-INPUT 0)
(define MAX-INPUT 10)


(define initial-world (make-world -10 
                                  10 
                                  -10
                                  10
                                  empty
                                  MIN-INPUT))

;; Computes distance.
;; distance: input input -> number
(define (input-distance x y)
  (abs (- x y)))

;; input-=?: input input -> boolean
(define (input=? x y)
  (equal? x y))

;; input->string: input -> string
(define (input->string x)
  (format "~v" x))

;; io->string: io string -> string
(define (io->string an-io)
  (string-append "f(" (input->string (io-input an-io))
                 ") = "
                 (posn->string (io-output an-io))))


;; posn->string: posn -> string
;; Converts a posn to a printable representation.
(define (posn->string a-posn)
  (string-append "(" (number->string (posn-x a-posn))
                 "," (number->string (posn-y a-posn))
                 ")"))



;; io-color: io number -> color
(define (io-color an-io input)
  (cond [(input=? (io-input an-io) input)
         (make-color 0 0 0)]
        [else
         (make-color (min (* (input-distance (io-input an-io) input) 50) 200)
                     (min (* (input-distance (io-input an-io) input) 50) 200)
                     (min (+ 50 (* (input-distance (io-input an-io) input) 50)) 255))]))
              

;; draw-arrow: posn posn scene -> scene
(define (draw-arrow from-pos to-pos a-scene)
  ;; fixme: do arrowhead later.
  (place-image (circle 5 "outline" "red")
               (posn-x to-pos)
               (posn-y to-pos)
               (place-image (line (- (posn-x to-pos)
                                     (posn-x from-pos))
                                  (- (posn-y to-pos)
                                     (posn-y from-pos))
                                  "black")
                            (posn-x from-pos)
                            (posn-y from-pos)
                            a-scene)))


;; render-canvas: world -> scene
;; Renders the canvas containing all the posns.
(define (render-canvas a-world)
  (render-canvas-ios a-world 
                        (world-ios a-world)
                        (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)))


;; world-defined-on-input?: world -> boolean
(define (world-defined-on-input? a-world)
  (input-in-ios? (world-current-input a-world)
                 (world-ios a-world)))


;; input-in-ios? input (listof io) -> boolean
(define (input-in-ios? an-input ios)
  (cond
    [(empty? ios)
     false]
    [(input=? an-input (io-input (first ios)))
     true]
    [else
     (input-in-ios? an-input (rest ios))]))



;; render-canvas-ios: world (listof ios) scene -> scene
(define (render-canvas-ios a-world ios a-scene)
  (cond
    [(empty? ios)
     a-scene]
    [else
     (render-canvas-ios a-world 
                           (rest ios)
                           (draw-canvas-posn a-world (first ios) a-scene))]))

;; draw-canvas-io: world posn scene -> scene
(define (draw-canvas-posn a-world an-io a-scene)
  (cond [(= (world-current-input a-world) (io-input an-io))
  
         (place-image (text (io->string an-io) 10 "purple")
                      (coordinate-x->canvas-x a-world (posn-x (io-output an-io)))
                      (coordinate-y->canvas-y a-world (posn-y (io-output an-io)))
                      (place-image (circle 2 "solid" (io-color an-io (world-current-input a-world)))
                                   (coordinate-x->canvas-x a-world (posn-x (io-output an-io)))
                                   (coordinate-y->canvas-y a-world (posn-y (io-output an-io)))
                                   a-scene))]
        [else
         (place-image (text (string-append "f(" (input->string (io-input an-io)) ")")
                            10 "lightgray")
                      (coordinate-x->canvas-x a-world (posn-x (io-output an-io)))
                      (coordinate-y->canvas-y a-world (posn-y (io-output an-io)))
                      
                      (place-image (circle 2 "solid" (io-color an-io (world-current-input a-world)))
                                   (coordinate-x->canvas-x a-world (posn-x (io-output an-io)))
                                   (coordinate-y->canvas-y a-world (posn-y (io-output an-io)))
                                   a-scene))]))
         
  


;; place-io: world number number -> world
(define (place-io a-world x y)
  (update-world-ios a-world (ios-insert/no-duplicates
                             (make-io (world-current-input a-world)
                                      (make-posn (canvas-x->coordinate-x a-world x)
                                                 (canvas-y->coordinate-y a-world y)))
                             (world-ios a-world))))


;; ios-insert/no-duplicates: io (listof io) -> (listof io)
(define (ios-insert/no-duplicates an-io other-ios)
  (cond
    [(empty? other-ios)
     (list an-io)]
    [(= (io-input an-io)
        (io-input (first other-ios)))
     (cons an-io (rest other-ios))]
    [else
     (cons (first other-ios)
           (ios-insert/no-duplicates an-io (rest other-ios)))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Translation between coordinate systems:

;; canvas-x->coordinate-x: world number -> number
(define (canvas-x->coordinate-x a-world a-canvas-x)
  (round (+ (world-x-min a-world) 
                     (* (/ a-canvas-x CANVAS-WIDTH)
                        (- (world-x-max a-world) (world-x-min a-world))))))


;; coordinate-x->canvas-x: world number -> number
(define (coordinate-x->canvas-x a-world a-coordinate-x)
  (* (- a-coordinate-x (world-x-min a-world))
     (/ CANVAS-WIDTH (- (world-x-max a-world) (world-x-min a-world)))))


;; canvas-y->coordinate-y: world number -> number
(define (canvas-y->coordinate-y a-world a-canvas-y)
  (round
   (+ (world-y-min a-world) 
      (* (/ (- CANVAS-HEIGHT a-canvas-y)
            CANVAS-HEIGHT)
         (- (world-y-max a-world) (world-y-min a-world))))))


;; coordinate-y->canvas-y: world number -> number
(define (coordinate-y->canvas-y a-world a-coordinate-y)
  (+ CANVAS-HEIGHT
     (/ (* (- (world-y-min a-world) a-coordinate-y)
           CANVAS-HEIGHT)
        (- (world-y-max a-world) (world-y-min a-world)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; on-clear-pressed
(define (on-clear-pressed a-world)
  (update-world-current-input
   (update-world-ios a-world empty)
   MIN-INPUT))


;; The view includes the canvas.  Clicks on the canvas add new posns.
(define view
  (col (canvas/callback render-canvas place-io)
       (slider world-current-input MIN-INPUT MAX-INPUT update-world-current-input)
       (button "Clear Canvas" on-clear-pressed)))


(big-bang initial-world view)