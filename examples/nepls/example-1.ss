;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname example-1) (read-case-sensitive #t) (teachpacks ((lib "world.ss" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "world.ss" "teachpack" "htdp")))))
;; Balloon and airplane

(define width 500)
(define height 500)

;; The world is the current time.
(define initial-world 0)

(define airplane-img (text "airplane" 10 "black")) ;; replace in slideshow
(define balloon-img  (text "balloon" 10 "black"))  ;; replace in slideshow

;; increment-time: world -> world
;; Increments the time in the world
(define (increment-time a-world)
  (add1 a-world))


;; render-world: world -> scene
;; Draws the plane and balloon in the scene.
(define (render-world a-world)
  (place-plane a-world
               (place-balloon a-world (empty-scene width height))))


;; place-plane: world scene -> scene
(define (place-plane a-world a-scene)
  (place-image airplane-img
               (* a-world 50)
               (image-height airplane-img) 
               a-scene))

;; place-balloon: world scene -> scene
(define (place-balloon a-world a-scene)
  (place-image balloon-img
               (- width (image-width balloon-img))
               (- height (* a-world 50))
               a-scene))


;; We render at 1 frames a second
(big-bang width height 1 initial-world)
(on-redraw render-world)
(on-tick-event increment-time)