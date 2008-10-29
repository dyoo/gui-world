;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname flag-1-update) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Flag designer.  Allows the user to design a flag.
(require "../../gui-world.ss")

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
;;                          misc-state
;;                          (one-of '("rect" "circle" "star"))
;;                          rect-state circle-state star-state)
(define-struct world (shapes 
                      misc-state 
                      current-shape 
                      rect-state
                      circle-state 
                      star-state))

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
              (make-misc-state (quotient FLAG-WIDTH 2) (quotient FLAG-HEIGHT 2) 0 0 0)
              "rect"
              (make-rect-state FLAG-WIDTH 10)
              (make-circle-state 30)
              (make-star-state 3 20 40)))
              

;; render-current-color: world -> scene
;; Draws a small square with the selected color.
(define (render-current-color a-misc-state)
  (place-image (nw:rectangle 30 30 "solid"
                             (misc-state-color (world-misc-state a-misc-state)))
               0 
               0
               (empty-scene 30 30)))


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
  (update-world-shapes a-world
                       (cons (world-current-editing-shape a-world)
                             (world-shapes a-world))))


;; render-flag: world -> scene
;; Draws the flag, given the list of its shapes.
(define (render-flag a-world)
  (render-shapes (world-shapes a-world)))

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



;; rect-width: world -> number
(define (rect-width a-world)
  (rect-state-width (world-rect-state a-world)))

;; update-rect-width: world number -> world
(define (update-rect-width a-world a-width)
  (update (rect-state-width (world-rect-state a-world))
          a-width))

;; rect-height: world -> number
(define (rect-height a-world)
  (rect-state-height (world-rect-state a-world)))

;; update-rect-height: world number -> world
(define (update-rect-height a-world a-height)
  (update (rect-state-height (world-rect-state a-world))
          a-height))

;; The rect tool part of the gui.
(define rect-gui
  (col (row "Width" (slider rect-width 1 FLAG-WIDTH update-rect-width))
       (row "Height" (slider rect-height 1 FLAG-WIDTH update-rect-height))))



;; circle-radius: world -> number
(define (circle-radius a-world)
  (circle-state-radius (world-circle-state a-world)))

;; update-circle-radius: world number -> world
(define (update-circle-radius a-world a-radius)
  (update (circle-state-radius (world-circle-state a-world))
          a-radius))

;; The circle part of the gui.
(define circle-gui
  (col (row "Radius" 
            (slider circle-radius 1 FLAG-WIDTH update-circle-radius))))



;; star-n: world -> number
(define (star-n a-world)
  (star-state-n (world-star-state a-world)))

;; update-star-n: world number -> world
(define (update-star-n a-world an-n)
  (update (star-state-n (world-star-state a-world))
          an-n))

;; star-inner-radius: world -> number
(define (star-inner-radius a-world)
  (star-state-inner-radius (world-star-state a-world)))

;; update-star-inner-radius: world number -> world
(define (update-star-inner-radius a-world an-n)
  (update (star-state-inner-radius (world-star-state a-world))
          an-n))

;; star-outer-radius: world -> number
(define (star-outer-radius a-world)
  (star-state-outer-radius (world-star-state a-world)))

;; update-star-outer-radius: world number -> world
(define (update-star-outer-radius a-world an-n)
  (update (star-state-outer-radius (world-star-state a-world))
          an-n))

;; The star part of the gui.
(define star-gui
  (col (row "# of points"
            (slider star-n 3 100 update-star-n))
       (row "Inner radius"
            (slider star-inner-radius 1 FLAG-WIDTH update-star-inner-radius))
       (row "Outer radius"
            (slider star-outer-radius 1 FLAG-WIDTH update-star-outer-radius))))




;; misc-x: world -> number
(define (misc-x a-world)
  (misc-state-x (world-misc-state a-world)))

;; update-misc-x: world number -> world
(define (update-misc-x a-world an-x)
  (update (misc-state-x (world-misc-state a-world))
          an-x))

;; misc-y: world -> number
(define (misc-y a-world)
  (misc-state-y (world-misc-state a-world)))

;; update-misc-y: world number -> world
(define (update-misc-y a-world a-y)
  (update (misc-state-y (world-misc-state a-world))
          a-y))

;; misc-r: world -> number
(define (misc-r a-world)
  (misc-state-r (world-misc-state a-world)))

;; update-misc-r: world number -> world
(define (update-misc-r a-world a-r)
  (update (misc-state-r (world-misc-state a-world))
          a-r))

;; misc-g: world -> number
(define (misc-g a-world)
  (misc-state-g (world-misc-state a-world)))

;; update-misc-g: world number -> world
(define (update-misc-g a-world an-g)
  (update (misc-state-g (world-misc-state a-world))
          an-g))

;; misc-b: world -> number
(define (misc-b a-world)
  (misc-state-b (world-misc-state a-world)))

;; update-misc-b: world number -> world
(define (update-misc-b a-world an-b)
  (update (misc-state-b (world-misc-state a-world))
          an-b))

;; The common attributes for all the shapes.
(define misc-gui
  (col (row "x" (slider misc-x 0 (sub1 FLAG-WIDTH) update-misc-x))
       (row "y" (slider misc-y 0 (sub1 FLAG-HEIGHT) update-misc-y))
       (canvas render-current-color)
       (row "r" (slider misc-r 0 255 update-misc-r))
       (row "g" (slider misc-g 0 255 update-misc-g))
       (row "b" (slider misc-b 0 255 update-misc-b))))


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
       (row (col (button/enabled "Select rect" 
                                 rect-button-pressed 
                                 world-rect-state-not-selected?)
                 (box-group/enabled "Rect options" 
                                    rect-gui
                                    world-rect-state-selected?))
            
            (col (button/enabled "Select circle" 
                                 circle-button-pressed 
                                 world-circle-state-not-selected?)
                 (box-group/enabled "Circle options"
                                    circle-gui
                                    world-circle-state-selected?))
            
            (col (button/enabled "Select star"
                                 star-button-pressed
                                 world-star-state-not-selected?)
                 (box-group/enabled "Star options" 
                                    star-gui
                                    world-star-state-selected?))
            
            misc-gui)
       (button button-label add-current-shape-to-flag)))

(big-bang initial-world a-main-gui)