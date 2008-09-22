#lang scheme/base

(require "gui-world.ss"
         htdp/image)

(define initial-world 20)


;; update-world-radius: world number -> world
;; Updates the radius of the circle to a-radius.
(define (update-world-radius a-world a-radius)
  a-radius)


(define (world->scene a-world)
  (make-form
   (place-image (circle a-world "solid" "red")
                100 100
                (empty-scene 200 200))
   (make-slider a-world 20 50 update-world-radius)))


(big-bang 100 100 initial-world)
(on-redraw world->scene)