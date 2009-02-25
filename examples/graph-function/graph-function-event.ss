#lang scheme
;; Graph function: representing partial math functions as graphs.
;; Difference equations.

(require "../../gui-world.ss")
(require lang/posn)


;; An io consists of an input and an output.
(define-struct io (input    ;; input
                   output   ;; posn
                   )
  #:prefab)
(define-updaters io)

(define-struct input (posn event) #:prefab)
(define-updaters input)
;; An event is one of the following:
(define event:??? "???")
(define event:left "left")
(define event:right "right")
(define event:up "up")
(define event:down "down")





;; A mode is one of the following: 
;;
;;     "create-io"
;;     "update-input"
;;     "update-output"
(define mode:create-io "create-io")
(define mode:update-input-position "update-input-position")
(define mode:update-output-position "update-output-position")



;;  A world consists of the bounds on the graph.
(define-struct world (x-min         ;; number
                      x-max         ;; number
                      y-min         ;; number
                      y-max         ;; number
                      ios           ;; (listof io)
                      mode
                      )
  #:prefab)
(define-updaters world)


(define CANVAS-WIDTH 300)
(define CANVAS-HEIGHT 300)

(define MIN-INPUT 0)
(define MAX-INPUT 10)

(define initial-world (make-world -10 
                                  10 
                                  -10
                                  10
                                  (list)
                                  mode:create-io))

;; posn=?: posn posn -> boolean
(define (posn=? p1 p2)
  (and 
   (= (posn-x p1) (posn-x p2))
   (= (posn-y p1) (posn-y p2))))


;; input-=?: input input -> boolean
(define (input=? input-1 input-2)
  (and (posn=? (input-posn input-1) 
               (input-posn input-2))
       (string=? (input-event input-1) 
                 (input-event input-2))))



;; posn->string: posn -> string
;; Converts a posn to a printable representation.
(define (posn->string a-posn)
  (string-append "(" (number->string (posn-x a-posn))
                 "," (number->string (posn-y a-posn))
                 ")"))




;; world-create-io: world number number -> world
(define (world-create-io a-world x y)
  (update-world-ios a-world (ios-insert/no-duplicates
                             (make-io (make-input (make-posn (canvas-x->coordinate-x a-world x)
                                                             (canvas-y->coordinate-y a-world y))
                                                  event:???)
                                      (make-posn (canvas-x->coordinate-x a-world x)
                                                 (canvas-y->coordinate-y a-world y)))
                             (world-ios a-world))))



;; world-delete-input: world input -> world
(define (world-delete-input a-world an-input)
  (update-world-ios a-world (delete-input-from-ios (world-ios a-world) an-input)))


;; delete-input-from-ios: (listof io) input -> (listof io)
(define (delete-input-from-ios ios an-input)
  (cond
    [(empty? ios)
     empty]
    [(input=? an-input (io-input (first ios)))
     (rest ios)]
    [else
     (cons (first ios)
           (delete-input-from-ios (rest ios) an-input))]))


;; world-update-current-input-position: world posn -> world
(define (world-update-current-input-position a-world a-posn)
  (update-world-ios a-world 
                    (cons (update-io-input (first (world-ios a-world))
                                           (update-input-posn (io-input (first (world-ios a-world)))
                                                              a-posn))
                          (rest (world-ios a-world)))))


;; world-update-current-input-event: world event -> world
(define (world-update-current-input-event a-world an-event)
  (update-world-ios a-world 
                    (cons (update-io-input (first (world-ios a-world))
                                           (update-input-event (io-input (first (world-ios a-world)))
                                                               an-event))
                          (rest (world-ios a-world)))))



;; world-update-current-output: world pons -> world
(define (world-update-current-output-position a-world a-posn)
  (update-world-ios a-world 
                    (cons (update-io-output (first (world-ios a-world))
                                            a-posn)
                          (rest (world-ios a-world)))))


;; world-has-ios?: world -> boolean
(define (world-has-ios? a-world)
  (not (empty? (world-ios a-world))))


;; world-choose-next-io: world -> world
(define (world-choose-next-io a-world)
  (update-world-ios a-world (append (rest (world-ios a-world))
                                    (list (first (world-ios a-world))))))

;; world-delete-current-io: world -> world
(define (world-delete-current-io a-world)
  (update-world-ios a-world (rest (world-ios a-world))))


;; ios-insert/no-duplicates: io (listof io) -> (listof io)
(define (ios-insert/no-duplicates an-io other-ios)
  (cons an-io 
        (delete-input-from-ios other-ios (io-input an-io))))



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


;; coordinate-posn->canvas-pons: world posn -> posn
(define (coordinate-posn->canvas-posn a-world a-posn)
  (make-posn (coordinate-x->canvas-x a-world (posn-x a-posn))
             (coordinate-y->canvas-y a-world (posn-y a-posn))))

;; canvas-posn->coordinate-posn: world posn -> posn
(define (canvas-posn->coordinate-posn a-world a-posn)
  (make-posn (canvas-x->coordinate-x a-world (posn-x a-posn))
             (canvas-y->coordinate-y a-world (posn-y a-posn))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; GUI code



;; render-canvas: world -> scene
;; Renders the canvas containing all the posns.
(define (render-canvas a-world)
  (draw-canvas-ios a-world
                   (world-ios a-world)
                   true
                   (empty-scene CANVAS-WIDTH CANVAS-HEIGHT)))


;; draw-canvas-ios: world (listof ios) scene -> scene
;; Draws all of the canvas ios.  The first drawn io is treated specially
(define (draw-canvas-ios a-world ios first-io? a-scene)
  (cond
    [(empty? ios)
     a-scene]
    [else
     (draw-canvas-ios a-world 
                      (rest ios) 
                      false
                      (draw-canvas-io a-world (first ios) first-io? a-scene))]))


;; place-image/posn: image posn scene -> scene
(define (place-image/posn an-image a-posn a-scene)
  (place-image an-image 
               (posn-x a-posn)
               (posn-y a-posn)
               a-scene))


;; draw-arrow/posn: posn posn color scene -> scene
(define (draw-arrow/posn posn-1 posn-2 color scene)
  (draw-arrow (posn-x posn-1) (posn-y posn-1) 
              (posn-x posn-2) (posn-y posn-2)
              color scene))

;; midpoint: posn posn -> posn
;; Computes the midpoint between two posns.
(define (midpoint posn-1 posn-2)
  (make-posn (/ (+ (posn-x posn-1) (posn-x posn-2)) 2)
             (/ (+ (posn-y posn-1) (posn-y posn-2)) 2)))


;; draw-canvas-io: world io scene -> scene
(define (draw-canvas-io a-world an-io first-io? a-scene)
  (cond [first-io?
         (place-image/posn
          (text (input-event (io-input an-io)) 10 "purple")
          (midpoint (coordinate-posn->canvas-posn a-world (io-output an-io))
                    (coordinate-posn->canvas-posn a-world (input-posn (io-input an-io))))
          (place-image/posn 
           (text (posn->string (io-output an-io)) 10 "purple")
           (coordinate-posn->canvas-posn a-world (io-output an-io))
           (place-image/posn 
            (text (posn->string (input-posn (io-input an-io))) 10 "purple")
            (coordinate-posn->canvas-posn a-world (input-posn (io-input an-io)))
            (draw-arrow/posn (coordinate-posn->canvas-posn a-world (input-posn (io-input an-io)))
                             (coordinate-posn->canvas-posn a-world (io-output an-io))
                             "black"
                             a-scene))))]
        [else
         (draw-arrow/posn (coordinate-posn->canvas-posn a-world (input-posn (io-input an-io)))
                          (coordinate-posn->canvas-posn a-world (io-output an-io))
                          "lightgray"
                          a-scene)]))



;; world-status-string: world -> string
(define (world-status-string a-world)
  (cond
    [(string=? (world-mode a-world) mode:create-io)
     "Creating a new case"]
    [(string=? (world-mode a-world) mode:update-input-position)
     "Updating the input"]
    [(string=? (world-mode a-world) mode:update-output-position)
     "Updating the output"]))


;; on-canvas-clicked: world number number -> world
(define (on-canvas-clicked a-world x y)
  (cond
    [(string=? (world-mode a-world) mode:create-io)
     (update-world-mode (world-create-io a-world x y)
                        mode:update-output-position)]
    [(string=? (world-mode a-world) mode:update-input-position)
     (world-update-current-input-position a-world
                                 (canvas-posn->coordinate-posn a-world (make-posn x y)))]
    [(string=? (world-mode a-world) mode:update-output-position)
     (world-update-current-output-position a-world
                                  (canvas-posn->coordinate-posn a-world (make-posn x y)))]))



;; on-create-pushed: world -> world
(define (on-create-pushed a-world)
  (update-world-mode a-world mode:create-io))

;; on-change-input-pushed: world -> world
(define (on-change-input-event-pushed a-world)
  (update-world-mode a-world mode:update-input-position))

;; on-next-pushed: world -> world
(define (on-next-pushed a-world)
  (world-choose-next-io a-world))

;; on-change-output-pushed: world -> world
(define (on-change-output-pushed a-world)
  (update-world-mode a-world mode:update-output-position))

;; on-delete-pushed: world -> world
(define (on-delete-pushed a-world)
  (update-world-mode (world-delete-current-io a-world) mode:create-io))

;; create-button-selectable?: world -> boolean
(define (create-button-selectable? a-world)
  (not (string=? (world-mode a-world) mode:create-io)))

;; input-button-selectable?: world -> boolean
(define (input-button-selectable? a-world)
  (and (not (empty? (world-ios a-world)))
       (not (string=? (world-mode a-world) mode:update-input-position))))

;; output-button-selectable?: world -> boolean
(define (output-button-selectable? a-world)
  (and (not (empty? (world-ios a-world)))
       (not (string=? (world-mode a-world) mode:update-output-position))))


;; world-input-event: world -> event
;; Returns the currently selected input event.
(define (world-input-event a-world)
  (cond
    [(empty? (world-ios a-world))
     event:???]
    [else
     (input-event (io-input (first (world-ios a-world))))]))



;; world-input-event-enabled?: world -> boolean
(define (world-input-event-enabled? a-world)
  (and (not (empty? (world-ios a-world)))
       (not (string=? (world-mode a-world) mode:create-io))))


;; input-event-choices: world -> (listof event)
(define (input-event-choices a-world)
  (list event:??? event:left event:right event:up event:down))


;; on-change-input-event: world event -> world
(define (on-change-input-event a-world an-event)
  (world-update-current-input-event a-world an-event))


;; The view includes the canvas.  Clicks on the canvas add new posns.
(define view
  (col (canvas/callback render-canvas on-canvas-clicked)
       (message world-status-string)
       (row (button/enabled "Create" on-create-pushed create-button-selectable?)
            (col (button/enabled "Change input position" on-change-input-event-pushed input-button-selectable?)
                 (drop-down/enabled world-input-event 
                                    input-event-choices
                                    on-change-input-event
                                    world-input-event-enabled?))
            (button/enabled "Change output" on-change-output-pushed output-button-selectable?))
       (button/enabled "Choose next case" on-next-pushed world-has-ios?)
       (button/enabled "Delete selected case" on-delete-pushed world-has-ios?)))



(big-bang initial-world view)


;
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Miscellaneous Snip support
;
;(define (world->thumbnail a-world)
;  (render-canvas a-world))
;
;
;;; world->syntax: world -> syntax
;;; Produces syntax from the world, if the world is to be treated as code.
;(define (world->syntax a-world)
;  (let ([body-f (lambda (x y)
;                  (let loop ([ios (world-ios a-world)])
;                    (cond 
;                      [(empty? ios)
;                       (error 'graph-function-difference
;                              "I don't know how to handle ~s ~s" x y)]
;                      [(input=? (io-input (first ios)) (make-posn x y))
;                       ;; We have to emit a value that the external user namespace
;                       ;; knows about.
;                       (let ([-make-posn (dynamic-require 'lang/posn 'make-posn)])
;                         (-make-posn (posn-x (io-output (first ios)))
;                                     (posn-y (io-output (first ios)))))]
;                      [else
;                       (loop (rest ios))])))])
;    (with-syntax ([body-f body-f]
;                  [x (datum->syntax #f 'x)]
;                  [y (datum->syntax #f 'y)])
;      (datum->syntax #f
;                     ;; This trickery is to make beginner-level happy with
;                     ;; the lambda that we're returning.
;                     ;; This is doing a 3d syntax thing.
;                     `(lambda (,#'x ,#'y)
;                        ,#'(body-f x y))))))
;
;
;;; world->bytes: world -> bytes
;(define (world->bytes a-world)
;  (match a-world
;    [(struct world (x-min x-max y-min y-max ios mode))
;     (let ([op (open-output-bytes a-world)])
;       (write (list x-min x-max y-min y-max
;                    (map io->sexp ios)
;                    mode)
;              op)
;       (get-output-bytes op))]))
;
;;; io->sexp: io -> sexp
;(define (io->sexp an-io)
;  (list (posn->sexp (io-input an-io))
;        (posn->sexp (io-output an-io))))
;
;;; posn->sexp: posn -> sexp
;(define (posn->sexp a-posn)
;  (list (posn-x a-posn)
;        (posn-y a-posn)))
;
;;; sexp->io: sexp->io
;(define (sexp->io an-sexp)
;  (match an-sexp
;    [(list input-pos-sexp output-pos-sexp)
;     (make-io (sexp->posn input-pos-sexp)
;              (sexp->posn output-pos-sexp))]))
;
;;; sexp->posn: sexp -> posn
;(define (sexp->posn an-sexp)
;  (match an-sexp
;    [(list x y)
;     (make-posn x y)]))
;
;;; bytes->world: bytes -> world
;(define (bytes->world some-bytes)
;  (match (read (open-input-bytes some-bytes))
;    [(list x-min x-max y-min y-max ios-sexp mode)
;     (make-world x-min x-max y-min y-max (map sexp->io ios-sexp) mode)]))
;
;
;(provide initial-world view world->syntax world->bytes bytes->world
;         world->thumbnail)