#lang scheme/base

(require "gui-world.ss"
         htdp/image)

(define initial-world 20)

(define (world->scene a-world)
  (make-form
   (circle a-world "solid" "red")
   (make-slider a-world 20 50)))

(big-bang 100 100 initial-world)

(on-redraw world->scene)