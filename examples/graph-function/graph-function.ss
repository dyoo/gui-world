;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname graph-function) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Graph function: representing partial math functions as graphs.

(require "../../gui-world.ss")

;; An io consists of an input and an output.
(define-struct io (input    ;; number 
                   output   ;; posn
                   ))
(define-updaters io)

;;  A world consists of the bounds on the graph.
(define-struct world (x-min         ;; number
                      x-max         ;; number
                      y-min         ;; number
                      y-max         ;; number
                      ios           ;; (listof io)
                      current-input ;; number
                      #;posns    ;; (listof posn)
                      #;selected-posn  ;; (or/c posn false)
                      ))
(define-updaters world)




(define initial-world (make-world -10 
                                  10 
                                  -10
                                  10
                                  empty
                                  0))

(define CANVAS-WIDTH 300)
(define CANVAS-HEIGHT 300)

(define MIN-INPUT 0)
(define MAX-INPUT 10)



;; render-canvas: world -> scene
;; Renders the canvas containing all the posns.
(define (render-canvas a-world)
  (render-canvas-ios a-world 
                        (world-ios a-world)
                        (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)))



;; render-canvas-ios: world (listof ios) scene -> scene
(define (render-canvas-ios a-world ios a-scene)
  (cond
    [(empty? ios)
     a-scene]
    [else
     (render-canvas-ios a-world 
                           (rest ios)
                           (draw-canvas-posn a-world (io-output (first ios)) a-scene))]))

;; draw-canvas-posn: world posn scene -> scene
(define (draw-canvas-posn a-world a-posn a-scene)
  (place-image (text (posn->string a-posn) 10 "purple")
               (coordinate-x->canvas-x a-world (posn-x a-posn))
               (coordinate-y->canvas-y a-world (posn-y a-posn))
               (place-image (circle 2 "solid" "black")
                            (coordinate-x->canvas-x a-world (posn-x a-posn))
                            (coordinate-y->canvas-y a-world (posn-y a-posn))
                            a-scene)))
  


;; any-shared-x?: posn (listof posn) -> boolean
(define (any-shared-x? p posns)
  (cond
    [(empty? posns)
     #f]
    [(and (not (equal? p (first posns)))
          (= (posn-x p)
             (posn-x (first posns))))
     #t]
    [else
     (any-shared-x? p (rest posns))]))




;; place-io: world number number -> world
(define (place-io a-world x y)
  (update-world-ios a-world (cons (make-io (world-current-input a-world)
                                           (make-posn (canvas-x->coordinate-x a-world x)
                                                      (canvas-y->coordinate-y a-world y)))
                                  (world-ios a-world))))


;; ios-insert/no-duplicates: io (listof io) -> (listof io)
#;(define (ios-insert/no-duplicates an-io other-posns)
  (cond
    [(empty? other-posns)
     (list a-posn)]
    [(equal? (first other-posns) a-posn)
     other-posns]
    [(< (posn-x a-posn)
        (posn-x (first other-posns)))
     (cons a-posn other-posns)]
    [else
     (cons (first other-posns)
           (ios-insert/no-duplicates a-posn (rest other-posns)))]))


;; delete: posn (listof posn) -> (listof posn)
#;(define (delete a-posn posns)
  (cond
    [(empty? posns)
     empty]
    [(equal? a-posn (first posns))
     (rest posns)]
    [else
     (cons (first posns)
           (delete a-posn (rest posns)))]))


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


;; posn->string: posn -> string
;; Converts a posn to a printable representation.
(define (posn->string a-posn)
  (string-append "(" (number->string (posn-x a-posn))
                 "," (number->string (posn-y a-posn))
                 ")"))


;; on-clear-pressed
(define (on-clear-pressed a-world)
  (update-world-current-input
   (update-world-ios a-world empty)
   0))





;; The view includes the canvas.  Clicks on the canvas add new posns.
(define view
  (col (canvas/callback render-canvas place-io)
       (slider world-current-input MIN-INPUT MAX-INPUT update-world-current-input)
       (button "Clear Canvas" on-clear-pressed)))


(big-bang initial-world view)