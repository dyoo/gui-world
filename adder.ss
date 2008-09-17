#lang scheme
(require "gui.ss")
(require htdp/world)

;; Simple counting example.  We show a text field and a button.  When the button is
;; pressed, we increment the number.


;; A world is a number.
(define initial-world 0)

;; on-button-click: world -> world
;; Increments the world when the button is pressed.
(define (on-button-click a-world)
  (+ a-world 1))


;; world->form-scene: world -> scene
;; Consumes the world, and produces a scene with a form.
(define (world->form-scene a-world)
  (make-form 
   (number->string a-world)
   (make-button "Press me!" on-button-click)))




(big-bang 100 300 1/20 initial-world)
(on-redraw world->form-scene)