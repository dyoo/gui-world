#lang scheme
(require htdp/world
         "gui.ss")

;; Calculator example.  A world is again a number
(define initial-world 0)


;; key0 through key9 correspond to the number pad keys.

;; key0: world -> world
(define (key0 a-world)
  (+ (* a-world 10) 0))

;; key1: world -> world
(define (key1 a-world)
  (+ (* a-world 10) 1))

;; key2: world -> world
(define (key2 a-world)
  (+ (* a-world 10) 2))

;; key3: world -> world
(define (key3 a-world)
  (+ (* a-world 10) 3))

;; key4: world -> world
(define (key4 a-world)
  (+ (* a-world 10) 4))

;; key5: world -> world
(define (key5 a-world)
  (+ (* a-world 10) 5))

;; key6: world -> world
(define (key6 a-world)
  (+ (* a-world 10) 6))

;; key7: world -> world
(define (key7 a-world)
  (+ (* a-world 10) 7))

;; key8: world -> world
(define (key8 a-world)
  (+ (* a-world 10) 8))

;; key9: world -> world
(define (key9 a-world)
  (+ (* a-world 10) 9))


;; key-negate: world -> world
;; Negate the value on screen.
(define (key-negate a-world)
  (- a-world))


;; key-square: world -> world
;; Square the value of the world.
(define (key-square a-world)
  (* a-world a-world))


;; world->form-scene: world -> scene
(define (world->form-scene a-world)
  (make-form (make-row (make-button "7" key7)
                       (make-button "8" key8)
                       (make-button "9" key9))
             (make-row (make-button "4" key4)
                       (make-button "5" key5)
                       (make-button "6" key6))
             (make-row (make-button "1" key1)
                       (make-button "2" key2)
                       (make-button "3" key3))
             (make-button "NEG" key-negate)
             (make-button "SQ" key-square)))

