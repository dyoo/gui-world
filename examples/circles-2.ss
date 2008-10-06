#lang scheme/base

(require "gui-world.ss"
         htdp/image)

;; The world is a ball with a given radius, x coordinate, and color.
(define-struct world (radius x color))
(define-updaters world)

;; The initial world is a red ball of radius 20, all the way to the left.
(define initial-world (make-world 20 0 "red"))


(define WIDTH 500)
(define HEIGHT 500)


;; color-ball-red: world -> world
;; Turns the ball red.
(define (color-ball-red a-world)
  (update-world-color a-world "red"))


;; color-ball-green: world -> world
;; Turns the ball green.
(define (color-ball-green a-world)
  (update-world-color a-world "green"))


;; color-ball-blue: world -> world
;; Turns the ball blue.
(define (color-ball-blue a-world)
  (update-world-color a-world "blue"))


;; move-ball: world -> world
;; Moves the ball toward the right, wrapping around the screen.
(define (move-ball a-world)
  (update-world-x a-world (modulo (+ (world-x a-world) 3)
                                  WIDTH)))

(define (render-ball a-world)
  (place-image (circle (world-radius a-world) "solid" (world-color a-world))
               (world-x a-world)
               (/ HEIGHT 2)
               (empty-scene WIDTH HEIGHT)))



;; make-button-enabled?: string -> (world -> boolean)
;; Given a function, produces a predicate that's true only when
;; the current ball's color is different from a-color.
(define ((make-button-enabled? a-color) a-world)
  (not (string=? (world-color a-world) a-color)))



;; Renders a gui that contains a form with a scene in it.
(define a-gui
  (col (scene render-ball)
       (row (button "Change to red" 
                    color-ball-red 
                    (make-button-enabled? "red"))
            
            (button "Change to green" 
                    color-ball-green
                    (make-button-enabled? "green"))
            
            (button "Change to blue"
                    color-ball-blue
                    (make-button-enabled? "blue")))
       (slider world-radius 20 50 update-world-radius)))


(big-bang initial-world a-gui)
(on-tick 1/20 move-ball)
