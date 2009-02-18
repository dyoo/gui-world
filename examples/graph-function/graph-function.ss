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
                      posns   ;; (listof posn)
                      ))
(define-updaters world)

(define initial-world (make-world -10 10 -10 10 empty))

(define CANVAS-WIDTH 300)
(define CANVAS-HEIGHT 300)



;; render-canvas: world -> scene
;; Renders the canvas containing all the posns.
(define (render-canvas a-world)
  (render-canvas-posns a-world 
                        (world-posns a-world)
                        (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)))



;; render-canvas-posns: world (listof posn) scene -> scene
(define (render-canvas-posns a-world posns a-scene)
  (cond
    [(empty? posns)
     a-scene]
    [else
     (render-canvas-posns a-world 
                           (rest posns)
                           (place-image (circle 5 "solid" "black")
                                        (coordinate-x->canvas-x a-world (posn-x (first posns)))
                                        (coordinate-y->canvas-y a-world (posn-y (first posns)))
                                        a-scene))]))


;; place-posn: world number number -> world
(define (place-posn a-world x y)
  (update-world-posns a-world
                       (cons (make-posn (canvas-x->coordinate-x a-world x)
                                        (canvas-y->coordinate-y a-world y))
                             (world-posns a-world))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Translation between coordinate systems:

;; canvas-x->coordinate-x: world number -> number
(define (canvas-x->coordinate-x a-world a-canvas-x)
  (+ (world-x-min a-world) 
     (* (/ a-canvas-x CANVAS-WIDTH)
        (- (world-x-max a-world) (world-x-min a-world)))))


;; coordinate-x->canvas-x: world number -> number
(define (coordinate-x->canvas-x a-world a-coordinate-x)
  (* (- a-coordinate-x (world-x-min a-world))
     (/ CANVAS-WIDTH (- (world-x-max a-world) (world-x-min a-world)))))


;; canvas-y->coordinate-y: world number -> number
(define (canvas-y->coordinate-y a-world a-canvas-y)
  (+ (world-y-min a-world) 
     (* (/ (- CANVAS-HEIGHT a-canvas-y)
           CANVAS-HEIGHT)
        (- (world-y-max a-world) (world-y-min a-world)))))


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

;; posns->string-list: (listof posn) -> (listof string)
(define (posns->string-list posns)
  (cond
    [(empty? posns)
     empty]
    [else
     (cons (posn->string (first posns))
           (posns->string-list (rest posns)))]))


;; world-posn-string-list: world -> (listof string)
(define (world-posn-string-list a-world)
  (cons "None selected"
        (posns->string-list (world-posns a-world))))

;; world-selected-posn-string: world -> string
(define (world-selected-posn-string a-world)
  "None selected")

;; on-posn-string-selected: world string -> world
(define (on-posn-string-selected a-world posn-string)
  a-world)


;; The view includes the canvas.  Clicks on the canvas add new posns.
(define view
  (col (canvas/callback render-canvas place-posn)
       (drop-down world-selected-posn-string world-posn-string-list on-posn-string-selected)))


(big-bang initial-world view)