;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname circles-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "../gui-world.ss")

;; The world is a ball with a given radius, x coordinate, and color.
(define-struct world (radius x color))
(define-updaters world)

;; The initial world is a red ball of radius 20, all the way to the left.
(define initial-world (make-world 20 0 "red"))


(define WIDTH 500)
(define HEIGHT 500)


; color-ball-red: world -> world
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



;; not-red-button-selected?: world -> boolean
;; Produces true when the red button is not selected.
(define (not-red-button-selected? a-world)
  (not (string=? (world-color a-world) "red")))

;; not-green-button-selected?: world -> boolean
;; Produces true when the green button is not selected.
(define (not-green-button-selected? a-world)
  (not (string=? (world-color a-world) "green")))

;; not-blue-button-selected?: world -> boolean
;; Produces true when the blue button is not selected.
(define (not-blue-button-selected? a-world)
  (not (string=? (world-color a-world) "blue")))



;; Renders a gui that contains a form with a scene in it.
(define a-gui
  (col (canvas render-ball)
       (row (button/enabled "Change to red" 
                            color-ball-red 
                            not-red-button-selected?)
            
            (button/enabled "Change to green" 
                            color-ball-green
                            not-green-button-selected?)
            
            (button/enabled "Change to blue"
                            color-ball-blue
                            not-blue-button-selected?))
       (slider world-radius 20 50 update-world-radius)))


(big-bang initial-world a-gui)
(on-tick 1/20 move-ball)
