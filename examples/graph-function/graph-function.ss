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
                      posns    ;; (listof posn)
                      selected-posn  ;; (or/c posn false)
                      ))
(define-updaters world)

(define initial-world (make-world -10 
                                  10 
                                  -10
                                  10
                                  empty
                                  false))

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
                           (draw-canvas-posn a-world (first posns) a-scene))]))

;; draw-canvas-posn: world posn scene -> scene
(define (draw-canvas-posn a-world a-posn a-scene)
  (place-image (text (posn->string a-posn) 10 "purple")
               (coordinate-x->canvas-x a-world (posn-x a-posn))
               (coordinate-y->canvas-y a-world (posn-y a-posn))
               (place-image (circle 2 "solid" (cond
                                                [(posn-makes-bad-function? a-world a-posn)
                                                 "red"]
                                                [else
                                                 "black"]))
                            (coordinate-x->canvas-x a-world (posn-x a-posn))
                            (coordinate-y->canvas-y a-world (posn-y a-posn))
                            a-scene)))
  

;; posn-makes-bad-function?: world posn -> boolean
;; Returns true if the posn shares an x coordinate with any other posn.
(define (posn-makes-bad-function? a-world a-posn)
  (any-shared-x? a-posn (world-posns a-world)))


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




;; place-posn: world number number -> world
(define (place-posn a-world x y)
  (update-world-selected-posn
   (update-world-posns a-world
                       (insert/no-duplicates (make-posn (canvas-x->coordinate-x a-world x)
                                                            (canvas-y->coordinate-y a-world y))
                                                 (world-posns a-world)))
   false))


;; insert/no-duplicates: posn (listof posn) -> (listof posn)
(define (insert/no-duplicates a-posn other-posns)
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
           (insert/no-duplicates a-posn (rest other-posns)))]))


;; delete: posn (listof posn) -> (listof posn)
(define (delete a-posn posns)
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
  (cond
    [(posn? (world-selected-posn a-world))
     (posn->string (world-selected-posn a-world))]
    [else
     "None selected"]))


;; on-posn-string-selected: world string -> world
(define (on-posn-string-selected a-world posn-string)
  (update-world-selected-posn 
   a-world
   (find-posn-with-posn-string (world-posns a-world) posn-string)))




;; find-posn-with-posn-string: (listof posn) string -> (or/c posn false)
(define (find-posn-with-posn-string posns a-posn-string)
  (cond
    [(empty? posns)
     #f]
    [(string=? (posn->string (first posns)) a-posn-string)
     (first posns)]
    [else
     (find-posn-with-posn-string (rest posns) a-posn-string)]))
     
               
;; posn-selected?: world -> boolean
;; Returns true if a point is selected.
(define (posn-selected? a-world)
  (posn? (world-selected-posn a-world)))


;; on-delete-pressed: world -> world
;; Deletes the selected point.
(define (on-delete-pressed a-world)
  (update-world-selected-posn
   (update-world-posns a-world (delete (world-selected-posn a-world) 
                                       (world-posns a-world)))
   false))

;; on-clear-pressed
(define (on-clear-pressed a-world)
  (update-world-selected-posn
   (update-world-posns a-world empty)
   false))

;; ignore-text-field-change: world string -> world
(define (ignore-text-field-change a-world a-new-text)
  a-world)


;; world->structured-posn-list-string: world -> string
(define (world->structured-posn-list-string a-world)
  (format "~v" (world-posns a-world)))


;; The view includes the canvas.  Clicks on the canvas add new posns.
(define view
  (col (canvas/callback render-canvas place-posn)
       (drop-down world-selected-posn-string world-posn-string-list on-posn-string-selected)
       (button/enabled "Delete" on-delete-pressed posn-selected?)
       (text-field world->structured-posn-list-string ignore-text-field-change)
       (button "Clear Canvas" on-clear-pressed)))


(big-bang initial-world view)