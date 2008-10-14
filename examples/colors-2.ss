#lang scheme/base
(require "../gui-world.ss")

(define-struct world (color1 color2 color3))
(define-updaters world)

;; render-color-block: color -> scene
;; Draws a small colored block.
(define (render-color-block a-color)
  (place-image (nw:rectangle 50 50 "solid" a-color)
               0 
               0
               (empty-scene 50 50)))

;; make-color-gui: (world -> color) (world color -> world) -> gui
(define (make-color-gui a-projection an-injection)
  (project/inject/gui (col (canvas render-color-block)
                           (row "R" (slider color-red 0 255 update-color-red))
                           (row "G" (slider color-green 0 255 update-color-green))
                           (row "B" (slider color-blue 0 255 update-color-blue)))
                      a-projection
                      an-injection))

;; update-color-red: color number -> color
(define (update-color-red a-color a-val)
  (make-color a-val
              (color-green a-color)
              (color-blue a-color)))


;; update-color-green: color number -> color
(define (update-color-green a-color a-val)
  (make-color (color-red a-color)
              a-val
              (color-blue a-color)))


;; update-color-blue: color number -> color
(define (update-color-blue a-color a-val)
  (make-color (color-red a-color)
              (color-green a-color)
              a-val))

(define main-gui
  (row (col "Color 1" (make-color-gui world-color1 update-world-color1))
       (col "Color 2" (make-color-gui world-color2 update-world-color2))
       (col "Color 3" (make-color-gui world-color3 update-world-color3))))

(big-bang (make-world (make-color 0 0 0) (make-color 0 0 0) (make-color 0 0 0))
          main-gui)