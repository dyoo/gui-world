#lang scheme/base

;; Flag designer.  Allows the user to design a flag.

(require "../../gui-world.ss")
(require scheme/list)

(define FLAG-WIDTH 500)
(define FLAG-HEIGHT (floor (/ FLAG-WIDTH 2)))



;; A shape is one of:
(define-struct rect-shape (posn color width height))
(define-struct circle-shape (posn color radius))
(define-struct star-shape (posn color n inner-radius outer-radius))
;; where posn is a posn, color is a color, width and height are
;; numbers, and radius, inner-radius, and outer-radius are numbers.


;; We also represent the state of the gui widgets.
(define-struct rect-state (width height))
(define-struct circle-state (radius))
(define-struct star-state (n inner-radius outer-radius))

;; Misc-state records the state that's generic across all the shapes.
;; x, y are coordinates
;; r, g, b is the color triple.
(define-struct misc-state (x y r g b))


;; A world is a (make-world (listof shape) 
;;                          (listof (listof shape))
;;                          number
;;                          misc-state
;;                          (one-of '("rect" "circle" "star"))
;;                          rect-state circle-state star-state)
(define-struct world (shapes              ;; list of the shapes on the flag
                      undo-history        ;; a history of the previous shapes.
                      misc-state          ;; the global miscellaneous state of the gui
                      current-shape       ;; the current shape type selected
                      rect-state          ;; the rect-state of the gui
                      circle-state        ;; the circle-state of the gui
                      star-state))        ;; the star-state of the gui.

;; Fixme: we should change define-struct to do this define-updaters business for us.
(define-updaters world)
(define-updaters rect-shape)
(define-updaters circle-shape)
(define-updaters star-shape)
(define-updaters rect-state)
(define-updaters circle-state)
(define-updaters star-state)
(define-updaters misc-state)


(define initial-world 
  (make-world empty
              empty
              (make-misc-state (quotient FLAG-WIDTH 2) (quotient FLAG-HEIGHT 2) 0 0 0)
              "rect"
              (make-rect-state FLAG-WIDTH 10)
              (make-circle-state 30)
              (make-star-state 3 20 40)))



;; render-current-color: misc-state -> scene
;; Draws a small square with the selected color.
(define (render-current-color a-misc-state)
  (place-image (nw:rectangle 30 30 "solid"
                             (misc-state-color a-misc-state))
               0 
               0
               (empty-scene 30 30)))


;; can-undo?: world -> boolean
;; Produces true if we can undo the world.
(define (can-undo? a-world)
  (not (empty? (world-undo-history a-world))))


;; undo: world -> world
;; Undoes the world, produces a world with the previous list of shapes.
(define (undo a-world)
  (update-world-shapes
   (update-world-undo-history a-world (rest (world-undo-history a-world)))
   (first (world-undo-history a-world))))



;; misc-state-color: misc-state -> color
(define (misc-state-color a-misc-state)
  (make-color (misc-state-r a-misc-state)
              (misc-state-g a-misc-state)
              (misc-state-b a-misc-state)))


;; misc-state-posn: misc-state -> posn
(define (misc-state-posn a-misc-state)
  (make-posn (misc-state-x a-misc-state)
             (misc-state-y a-misc-state)))


;; world-current-editing-shape: world -> shape
;; Gets the shape that is currently selected and being edited by the user.
(define (world-current-editing-shape a-world)
  (cond
    [(string=? (world-current-shape a-world) "rect")
     (make-rect-shape (misc-state-posn (world-misc-state a-world))
                      (misc-state-color (world-misc-state a-world))
                      (rect-state-width (world-rect-state a-world))
                      (rect-state-height (world-rect-state a-world)))]
    
    [(string=? (world-current-shape a-world) "circle")
     (make-circle-shape (misc-state-posn (world-misc-state a-world))
                        (misc-state-color (world-misc-state a-world))
                        (circle-state-radius (world-circle-state a-world)))]

    [(string=? (world-current-shape a-world) "star")
     (make-star-shape (misc-state-posn (world-misc-state a-world))
                      (misc-state-color (world-misc-state a-world))
                      (star-state-n (world-star-state a-world))
                      (star-state-inner-radius (world-star-state a-world))
                      (star-state-outer-radius (world-star-state a-world)))]))


;; add-current-shape-to-flag: world -> world
;; Adds a new shape to the flag.
(define (add-current-shape-to-flag a-world)
  (update-world-undo-history 
   (update-world-shapes a-world
                        (cons (world-current-editing-shape a-world)
                              (world-shapes a-world)))
   (cons (world-shapes a-world)
         (world-undo-history a-world))))


;; render-flag: world -> scene
;; Draws the flag, given the list of its shapes.
(define (render-flag a-world)
  (draw-cross-cursor (misc-state-posn (world-misc-state a-world)) 
                     (render-shapes
                      (cons (world-current-editing-shape a-world)
                            (world-shapes a-world)))))



;; render-shapes: (listof shape) -> scene
;; Draws all of the flag shapes onto an empty scene.
(define (render-shapes shapes)
  (cond [(empty? shapes)
         (empty-scene FLAG-WIDTH FLAG-HEIGHT)]
        [else
         (draw-shape (first shapes)
                     (render-shapes (rest shapes)))]))

;; draw-shape: shape scene -> scene
;; Draws a single shape onto the scene
(define (draw-shape a-shape a-scene)
  (cond
    [(rect-shape? a-shape)
     (place-image (nw:rectangle (rect-shape-width a-shape) 
                                (rect-shape-height a-shape)
                                "solid"
                                (rect-shape-color a-shape))
                  (posn-x (rect-shape-posn a-shape))
                  (posn-y (rect-shape-posn a-shape))
                  a-scene)]
    [(circle-shape? a-shape)
     (place-image (circle (circle-shape-radius a-shape) 
                          "solid"
                          (circle-shape-color a-shape))
                  (posn-x (circle-shape-posn a-shape))
                  (posn-y (circle-shape-posn a-shape))
                  a-scene)]
    [(star-shape? a-shape)
     (place-image (star (star-shape-n a-shape)
                        (star-shape-inner-radius a-shape) 
                        (star-shape-outer-radius a-shape) 
                        "solid"
                        (star-shape-color a-shape))
                  (posn-x (star-shape-posn a-shape))
                  (posn-y (star-shape-posn a-shape))
                  a-scene)]))


;; draw-cross-cursor: posn scene -> scene
;; Adds a little cross cursor mark at a-posn on a-scene.
(define CROSS-SIZE 10)
(define (draw-cross-cursor a-posn a-scene)
  (place-image (nw:rectangle CROSS-SIZE 1 "solid" "black")
               (- (posn-x a-posn) (quotient CROSS-SIZE 2))
               (posn-y a-posn)
               (place-image (nw:rectangle 1 CROSS-SIZE "solid" "black")
                            (posn-x a-posn)
                            (- (posn-y a-posn) (quotient CROSS-SIZE 2))
                            a-scene)))


;                       
;                       
;                       
;     ;;;  ;    ; ;;;;; 
;    ;   ; ;    ;   ;   
;   ;      ;    ;   ;   
;   ;      ;    ;   ;   
;   ;   ;; ;    ;   ;   
;   ;    ; ;    ;   ;   
;   ;    ; ;    ;   ;   
;    ;   ; ;    ;   ;   
;     ;;;   ;;;;  ;;;;; 
;                       
;                       
;                ; ;  ;;



;; The rect tool part of the gui.
(define rect-gui
  (col (row "Width" (slider rect-state-width 1 FLAG-WIDTH update-rect-state-width))
       (row "Height" (slider rect-state-height 1 FLAG-WIDTH update-rect-state-height))))


;; The circle part of the gui.
(define circle-gui
  (col (row "Radius" (slider circle-state-radius 1 FLAG-WIDTH update-circle-state-radius))))

;; The star part of the gui.
(define star-gui
  (col (row "# of points"
            (slider star-state-n 3 100 update-star-state-n))
       (row "Inner radius"
            (slider star-state-inner-radius 1 FLAG-WIDTH update-star-state-inner-radius))
       
       (row "Outer radius"
            (slider star-state-outer-radius 1 FLAG-WIDTH update-star-state-outer-radius))))

;; The common attributes for all the shapes.
(define misc-gui
  (col (row "x" (slider misc-state-x 0 (sub1 FLAG-WIDTH) update-misc-state-x))
       (row "y" (slider misc-state-y 0 (sub1 FLAG-HEIGHT) update-misc-state-y))
       (canvas render-current-color)
       (row "r" (slider misc-state-r 0 255 update-misc-state-r))
       (row "g" (slider misc-state-g 0 255 update-misc-state-g))
       (row "b" (slider misc-state-b 0 255 update-misc-state-b))))


;; rect-button-pressed: world -> world
(define (rect-button-pressed a-world)
  (update-world-current-shape a-world "rect"))

;; circle-button-pressed: world -> world
(define (circle-button-pressed a-world)
  (update-world-current-shape a-world "circle"))

;; star-button-pressed: world -> world
(define (star-button-pressed a-world)
  (update-world-current-shape a-world "star"))


;; world-rect-state-not-selected?: world -> boolean
(define (world-rect-state-not-selected? a-world)
  (not (string=? (world-current-shape a-world) "rect")))

;; world-circle-state-not-selected?: world -> boolean
(define (world-circle-state-not-selected? a-world)
  (not (string=? (world-current-shape a-world) "circle")))

;; world-star-state-not-selected?: world -> boolean
(define (world-star-state-not-selected? a-world)
  (not (string=? (world-current-shape a-world) "star")))


;; world-rect-state-selected?: world -> boolean
(define (world-rect-state-selected? a-world)
  (string=? (world-current-shape a-world) "rect"))

;; world-circle-state-selected?: world -> boolean
(define (world-circle-state-selected? a-world)
  (string=? (world-current-shape a-world) "circle"))

;; world-star-state-selected?: world -> boolean
(define (world-star-state-selected? a-world)
  (string=? (world-current-shape a-world) "star"))



;; button-label: world -> string
;; Produces the label we attach to the add button in our gui.
(define (button-label a-world)
  (string-append "Add " (world-current-shape a-world) "!"))


;; The main gui combines all of the shape controls.
(define a-main-gui
  (col (canvas render-flag)
       (row (col (button "Select rect" 
                         rect-button-pressed 
                         world-rect-state-not-selected?)
                 (group-box "Rect options" 
                            (scope-struct rect-gui world-rect-state update-world-rect-state)
                            world-rect-state-selected?))
            
            (col (button "Select circle" 
                         circle-button-pressed 
                         world-circle-state-not-selected?)
                 (group-box "Circle options"
                            (scope-struct circle-gui world-circle-state update-world-circle-state)
                            world-circle-state-selected?))
            
            (col (button "Select star"
                         star-button-pressed
                         world-star-state-not-selected?)
                 (group-box "Star options" 
                            (scope-struct star-gui world-star-state update-world-star-state)
                            world-star-state-selected?))
            
            (scope-struct misc-gui world-misc-state update-world-misc-state))
       (button button-label add-current-shape-to-flag)
       (button "Undo" undo can-undo?)))


(big-bang initial-world a-main-gui)
