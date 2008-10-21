;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname circles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "../gui-world.ss")

(define initial-world 20)

;; update-world-radius: world number -> world
;; Updates the radius of the circle to a-radius.
(define (update-world-radius a-world a-radius)
  a-radius)

;; world-radius: world -> number
;; Produces the radius of the world.
(define (world-radius a-world)
  a-world)


;; render-ball: world -> scene
;; Renders the ball at the world's radius.
(define (render-ball a-world)
  (place-image (circle (world-radius a-world) "solid" "red")
               100 100
               (empty-scene 200 200)))

(define a-gui
  (col (canvas render-ball)
       (slider world-radius 20 50 update-world-radius)))


(big-bang initial-world a-gui)