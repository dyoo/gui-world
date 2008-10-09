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
(define (render-current-color a-world)
  (place-image (nw:rectangle 30 30 "solid"
                             (make-color (misc-state-r (world-misc-state a-world))
                                         (misc-state-g (world-misc-state a-world))
                                         (misc-state-b (world-misc-state a-world))))
               0 
               0
               (empty-scene 30 30)))



;; world-current-color: world -> color
(define (world-current-color a-world)
  (make-color (misc-state-r (world-misc-state a-world))
              (misc-state-g (world-misc-state a-world))
              (misc-state-b (world-misc-state a-world))))

;; world-current-posn: world -> posn
(define (world-current-posn a-world)
  (make-posn (misc-state-x (world-misc-state a-world))
             (misc-state-y (world-misc-state a-world))))


;; world-current-editing-shape: world -> shape
;; Gets the shape that is currently selected and being edited by the user.
(define (world-current-editing-shape a-world)
  (cond
    [(string=? (world-current-shape a-world) "rect")
     (make-rect-shape (world-current-posn a-world)
                      (world-current-color a-world)
                      (rect-state-width (world-rect-state a-world))
                      (rect-state-height (world-rect-state a-world)))]
    
    [(string=? (world-current-shape a-world) "circle")
     (make-circle-shape (world-current-posn a-world)
                        (world-current-color a-world)
                        (circle-state-radius (world-circle-state a-world)))]

    [(string=? (world-current-shape a-world) "star")
     (make-star-shape (world-current-posn a-world)
                      (world-current-color a-world)
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
;                 ;;;                               
;   ;    ;          ;                               
;   ;    ;          ;                               
;   ;    ;  ;;;     ;    ;;;;    ;;;    ; ;;   ;;;  
;   ;    ; ;;  ;    ;    ;; ;;  ;;  ;   ;;  ; ;   ; 
;   ;;;;;; ;   ;    ;    ;   ;  ;   ;   ;     ;     
;   ;    ; ;;;;;    ;    ;   ;  ;;;;;   ;      ;;;  
;   ;    ; ;        ;    ;   ;  ;       ;         ; 
;   ;    ; ;;  ;    ;    ;; ;;  ;;  ;   ;     ;   ; 
;   ;    ;  ;;;      ;;  ;;;;    ;;;    ;      ;;;  
;                        ;                          
;                        ;                          
;                        ;                  ;; ; ;; 





;; world-rect-state-width: world -> number
;; Consumes the global state of the world, produces the width in the rect-state.
(define (world-rect-state-width a-world)
  (rect-state-width (world-rect-state a-world)))


;; update-world-rect-state-width: world number -> world
;; Updates the rect width portion of the global state.
(define (update-world-rect-state-width a-world width)
  (update-world-rect-state a-world
                           (update-rect-state-width (world-rect-state a-world)
                                                    width)))


;; world-rect-state-height: world -> number
;; Consumes the global state of the world, produces the height in the rect-state.
(define (world-rect-state-height a-world)
  (rect-state-height (world-rect-state a-world)))


;; update-world-rect-state-width: world number -> world
;; Updates the rect width portion of the global state.
(define (update-world-rect-state-height a-world width)
  (update-world-rect-state a-world
                           (update-rect-state-height (world-rect-state a-world)
                                                     width)))


;; world-circle-state-radius: world -> number
;; Produces the circle radius of the circle-gui part of the global state.
(define (world-circle-state-radius a-world)
  (circle-state-radius
   (world-circle-state a-world)))


;; update-world-circle-state-radius: world number -> world
;; Updates the circle radius of the circle-gui part of the global state.
(define (update-world-circle-state-radius a-world a-radius)
  (update-world-circle-state a-world
                             (update-circle-state-radius (world-circle-state a-world)
                                                         a-radius)))

;; world-star-state-n: world -> number
(define (world-star-state-n a-world)
  (star-state-n
   (world-star-state a-world)))


;; update-world-star-state-n: world number -> world
(define (update-world-star-state-n a-world n)
  (update-world-star-state a-world
                           (update-star-state-n (world-star-state a-world)
                                                n)))


;; world-star-state-inner-radius: world -> number
(define (world-star-state-inner-radius a-world)
  (star-state-inner-radius
   (world-star-state a-world)))


;; update-world-star-state-outer-radius: world number -> world
(define (update-world-star-state-inner-radius a-world inner-radius)
  (update-world-star-state a-world
                           (update-star-state-inner-radius (world-star-state a-world)
                                                           inner-radius)))


;; world-star-state-outer-radius: world -> number
(define (world-star-state-outer-radius a-world)
  (star-state-outer-radius
   (world-star-state a-world)))

;; update-world-star-state-outer-radius
(define (update-world-star-state-outer-radius a-world outer-radius)
  (update-world-star-state a-world
                           (update-star-state-outer-radius (world-star-state a-world)
                                                           outer-radius)))


;; world-misc-state-x: world -> number
(define (world-misc-state-x a-world)
  (misc-state-x 
   (world-misc-state a-world)))


;; update-world-misc-state-x: world number -> world
(define (update-world-misc-state-x a-world x)
  (update-world-misc-state a-world
                           (update-misc-state-x (world-misc-state a-world) x)))


;; world-misc-state-y: world -> number
(define (world-misc-state-y a-world)
  (misc-state-y 
   (world-misc-state a-world)))


;; update-world-misc-state-y: world number -> world
(define (update-world-misc-state-y a-world y)
  (update-world-misc-state a-world
                           (update-misc-state-y (world-misc-state a-world) y)))

;; world-misc-state-r: world -> number
(define (world-misc-state-r a-world)
  (misc-state-r
   (world-misc-state a-world)))


;; update-world-misc-state-r: world number -> world
(define (update-world-misc-state-r a-world r)
  (update-world-misc-state a-world
                           (update-misc-state-r (world-misc-state a-world) r)))


;; world-misc-state-g: world -> number
(define (world-misc-state-g a-world)
  (misc-state-g 
   (world-misc-state a-world)))


;; update-world-misc-state-g: world number -> world
(define (update-world-misc-state-g a-world g)
  (update-world-misc-state a-world
                           (update-misc-state-g (world-misc-state a-world) g)))


;; world-misc-state-b: world -> number
(define (world-misc-state-b a-world)
  (misc-state-b 
   (world-misc-state a-world)))

;; update-world-misc-state-b: world number -> world
(define (update-world-misc-state-b a-world b)
  (update-world-misc-state a-world
                           (update-misc-state-b (world-misc-state a-world) b)))




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
  (col (row "Width" (slider world-rect-state-width 1 FLAG-WIDTH 
                            update-world-rect-state-width))
       (row "Height" (slider world-rect-state-height 1 FLAG-WIDTH 
                             update-world-rect-state-height))))


;; The circle part of the gui.
(define circle-gui
  (col (row "Radius" 
            (slider world-circle-state-radius 
                    1
                    FLAG-WIDTH 
                    update-world-circle-state-radius))))

;; The star part of the gui.
(define star-gui
  (col (row "# of points"
            (slider world-star-state-n 3 100 update-world-star-state-n))
       (row "Inner radius"
            (slider world-star-state-inner-radius
                    1 FLAG-WIDTH 
                    update-world-star-state-inner-radius))
       
       (row "Outer radius"
            (slider world-star-state-outer-radius 
                    1 FLAG-WIDTH 
                    update-world-star-state-outer-radius))))

;; The common attributes for all the shapes.
(define misc-gui
  (col (row "x" (slider world-misc-state-x 0 (sub1 FLAG-WIDTH) update-world-misc-state-x))
       (row "y" (slider world-misc-state-y 0 (sub1 FLAG-HEIGHT) update-world-misc-state-y))
       (scene render-current-color)
       (row "r" (slider world-misc-state-r 0 255 update-world-misc-state-r))
       (row "g" (slider world-misc-state-g 0 255 update-world-misc-state-g))
       (row "b" (slider world-misc-state-b 0 255 update-world-misc-state-b))))


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
  (col (scene render-flag)
       (row (col (button "Select rect" 
                         rect-button-pressed 
                         world-rect-state-not-selected?)
                 (group-box "Rect options" 
                            rect-gui
                            world-rect-state-selected?))
            
            (col (button "Select circle" 
                         circle-button-pressed 
                         world-circle-state-not-selected?)
                 (group-box "Circle options"
                            circle-gui
                            world-circle-state-selected?))
            
            (col (button "Select star"
                         star-button-pressed
                         world-star-state-not-selected?)
                 (group-box "Star options" 
                            star-gui
                            world-star-state-selected?))
            
            misc-gui)
       (button button-label add-current-shape-to-flag)))

(big-bang initial-world a-main-gui)