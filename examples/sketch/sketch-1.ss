;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sketch-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "../../gui-world.ss")


;; The width and height of the drawing canvas.
(define WIDTH 500)
(define HEIGHT 300)
(define BLANK-COLOR "lightgray")
(define DRAW-COLOR "darkgray")
(define DOT-RADIUS 3)


;; A world will be the current position, and a list of posns representing
;; the dots drawn already.
(define-struct world (posn dots))
(define-updaters world)


;; We start things off by putting the position at the very center, on an empty
;; canvas.
(define initial-world 
  (make-world (make-posn (/ WIDTH 2)
                         (/ HEIGHT 2))
              empty))


;; draw-world-posn: posn scene -> scene
;; Draws the pin posn onto the scene.
(define (draw-world-posn a-posn a-scene)
  (place-image (nw:rectangle 1 3 "solid" "black")
               (posn-x a-posn)
               (posn-y a-posn)
               a-scene))


;; draw-dots: (listof posn) scene -> scene
;; Draws the dots onto a-scene.
(define (draw-dots dots a-scene)
  (cond
    [(empty? dots)
     a-scene]
    [else
     (draw-dots (rest dots)
                (place-image (circle DOT-RADIUS "solid" DRAW-COLOR)
                             (posn-x (first dots))
                             (posn-y (first dots))
                             a-scene))]))


;; add-posn-to-dots: world -> world
(define (add-posn-to-dots a-world)
  (update-world-dots a-world
                     (cons (world-posn a-world)
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


;; a-gui: gui
(define a-gui
  (col
   (canvas render-etch-a-sketch)
   (button "up" move-up)
   (row (button "left" move-left)
        (button "right" move-right))
   (button "down" move-down)
   (button "shake!" shake-to-clear)))

(big-bang initial-world a-gui)