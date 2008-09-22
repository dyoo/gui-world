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


;; world->scene: world -> form-scene
;; Renders a gui that contains a form with a scene in it.
(define (world->scene a-world)
  (make-form
   (place-image (circle (world-radius a-world) "solid" (world-color a-world))
                (world-x a-world)
                (/ HEIGHT 2)
                (empty-scene WIDTH HEIGHT))
   (make-row (make-button "Change to red" 
                          color-ball-red 
                          (not (string=? (world-color a-world) "red")))
             (make-button "Change to green" 
                          color-ball-green
                          (not (string=? (world-color a-world) "green")))
             (make-button "Change to blue"
                          color-ball-blue
                          (not (string=? (world-color a-world) "blue"))))
   (make-slider (world-radius a-world) 20 50 update-world-radius)))


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


(big-bang 100 100 initial-world)
(on-tick 1/20 move-ball)

(on-redraw world->scene)