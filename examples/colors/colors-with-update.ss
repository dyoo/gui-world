;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname colors-with-update) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "../../gui-world.ss")

(define-struct world (color-1 color-2 color-3))
(define-updaters world)


;; make-render-color-block: (world -> color) -> (world -> scene)
;; Draws a small colored block.
(define (make-render-color-block get-color)  
  (local [(define (make-a-scene a-world)
            (place-image (nw:rectangle 50 50 "solid" (get-color a-world))
                         0 
                         0
                         (empty-scene 50 50)))]
    make-a-scene))

;; make-color-gui: (world -> color) (world color -> world) -> gui
(define (make-color-gui world-color update-world-color)
  (local [(define (world-color-red a-world)
            (color-red (world-color a-world)))

          (define (world-color-green a-world)
            (color-green (world-color a-world)))

          (define (world-color-blue a-world)
            (color-blue (world-color a-world)))

          (define (update-world-color-red a-world a-val)
            (with-accessor/updater 
             ([world-color update-world-color])
             (update (color-red (world-color a-world)) a-val)))
          
          (define (update-world-color-green a-world a-val)
            (with-accessor/updater
             ([world-color update-world-color])
             (update (color-green (world-color a-world))
                     a-val)))
            
          (define (update-world-color-blue a-world a-val)
            (with-accessor/updater
             ([world-color update-world-color])
             (update (color-blue (world-color a-world))
                     a-val)))]

    (col (canvas (make-render-color-block world-color))
         (row "R" 
              (slider world-color-red 0 255 update-world-color-red))
         (row "G" 
              (slider world-color-green 0 255 update-world-color-green))
         (row "B" 
              (slider world-color-blue 0 255 update-world-color-blue)))))


(define main-gui
  (row (col "Color 1" (make-color-gui world-color-1 update-world-color-1))
       (col "Color 2" (make-color-gui world-color-2 update-world-color-2))
       (col "Color 3" (make-color-gui world-color-3 update-world-color-3))))


(big-bang (make-world (make-color 0 0 0) 
                      (make-color 0 0 0)
                      (make-color 0 0 0))
          main-gui)