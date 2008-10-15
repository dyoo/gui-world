#lang scheme/base
(require "../../gui-world.ss"
         scheme/list)

;; The width and height of the drawing canvas.
(define WIDTH 500)
(define HEIGHT 300)
(define BLANK-COLOR "lightgray")
(define DOT-RADIUS 3)

(define COLORS (list "red" "orange" "yellow" "green" "blue" "violet" "darkgray"))


;; A dot is a
;; (make-dot posn color)
(define-struct dot (posn color))
(define-updaters dot)

;; A world is a 
;; (make-world posn (listof dot) (one-of "left" "right" "up" "down" "stable") string)
(define-struct world (posn dots direction current-color))
(define-updaters world)


;; We start things off by putting the position at the very center, on an empty
;; canvas, with no drifting.
(define initial-world 
  (make-world (make-posn (/ WIDTH 2)
                         (/ HEIGHT 2))
              empty
              "stable"
              "darkgray"))


;; draw-world-posn: posn scene -> scene
;; Draws the pin posn onto the scene.
(define (draw-world-posn a-posn a-scene)
  (place-image (nw:rectangle 1 3 "solid" "black")
               (posn-x a-posn)
               (posn-y a-posn)
               a-scene))


;; draw-dots: (listof dot) scene -> scene
;; Draws the dots onto a-scene.
(define (draw-dots dots  a-scene)
  (cond
    [(empty? dots)
     a-scene]
    [else
     (place-image (circle DOT-RADIUS "solid" (dot-color (first dots)))
                  (posn-x (dot-posn (first dots)))
                  (posn-y (dot-posn (first dots)))
                  (draw-dots (rest dots) a-scene))]))


;; add-posn-to-dots: world -> world
(define (add-posn-to-dots a-world)
  (update-world-dots a-world
                     (cons (make-dot (world-posn a-world) (world-current-color a-world))
                           (world-dots a-world))))

;; move-left: world -> world
(define (move-left a-world)
  (add-posn-to-dots
   (update-world-posn a-world 
                      (update-posn-x (world-posn a-world)
                                     (max 0
                                          (- (posn-x (world-posn a-world)) DOT-RADIUS))))))

;; move-right: world -> world
(define (move-right a-world)
  (add-posn-to-dots
   (update-world-posn a-world
                      (update-posn-x (world-posn a-world)
                                     (min (sub1 WIDTH)
                                          (+ (posn-x (world-posn a-world)) DOT-RADIUS))))))
;; move-up: world -> world
(define (move-up a-world)
  (add-posn-to-dots
   (update-world-posn a-world
                      (update-posn-y (world-posn a-world)
                                     (max 0
                                          (- (posn-y (world-posn a-world)) DOT-RADIUS))))))
;; move-down: world -> world
(define (move-down a-world)
  (add-posn-to-dots
   (update-world-posn a-world
                      (update-posn-y (world-posn a-world)
                                     (min (sub1 HEIGHT)
                                          (+ (posn-y (world-posn a-world)) DOT-RADIUS))))))


;; move-by-drifting: world -> world
;; Move the pin based on the current drifting direction.
(define (move-by-drifting a-world)
  (cond
    [(string=? (world-direction a-world) "stable")
     a-world]
    [(string=? (world-direction a-world) "left")
     (move-left a-world)]
    [(string=? (world-direction a-world) "right")
     (move-right a-world)]
    [(string=? (world-direction a-world) "up")
     (move-up a-world)]
    [(string=? (world-direction a-world) "down")
     (move-down a-world)]))
               

;; shake-to-clear: world -> world
;; Clears out all the dots on the canvas, but leaves the position preserved.
(define (shake-to-clear a-world)
  (update-world-dots a-world empty))



;; render-etch-a-sketch: world -> scene
;; Draws the etch-a-sketch's canvas
(define (render-etch-a-sketch a-world)
  (draw-world-posn (world-posn a-world)
                   (draw-dots (world-dots a-world)
                              (place-image (nw:rectangle WIDTH HEIGHT "solid" BLANK-COLOR)
                                           0
                                           0
                                           (empty-scene WIDTH HEIGHT)))))

;; change-direction-left: world -> world
(define (change-direction-left a-world)
  (update-world-direction a-world "left"))

;; change-direction-right: world -> world
(define (change-direction-right a-world)
  (update-world-direction a-world "right"))

;; change-direction-up: world -> world
(define (change-direction-up a-world)
  (update-world-direction a-world "up"))

;; change-direction-down: world -> world
(define (change-direction-down a-world)
  (update-world-direction a-world "down"))

;; change-direction-stable: world -> world
(define (change-direction-stable a-world)
  (update-world-direction a-world "stable"))
  

;; a-gui: gui
(define a-gui
  (col
   (canvas render-etch-a-sketch)
   (button "up" change-direction-up)
   (row (button "left" change-direction-left)
        (button "stop" change-direction-stable)
        (button "right" change-direction-right))
   (button "down" change-direction-down)
   (button "shake!" shake-to-clear)
   (drop-down world-current-color COLORS update-world-current-color)))

(big-bang initial-world a-gui)
(on-tick 1/20 move-by-drifting)