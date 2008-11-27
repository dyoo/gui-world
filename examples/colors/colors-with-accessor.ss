;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname colors-with-accessor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "../../gui-world.ss")

(define-struct world (color-1 color-2 color-3))
(define-updaters world)


;; make-render-color-block: accessor -> (world -> scene)
;; Draws a small colored block.
(define (make-render-color-block color-accessor)  
  (local [(define (make-a-scene a-world)
            (place-image (nw:rectangle 50 50 "solid" (get/accessor color-accessor a-world))
                         0 
                         0
                         (empty-scene 50 50)))]
    make-a-scene))

;; make-color-gui: accessor -> gui
(define (make-color-gui world-color-accessor)
  (local [(define (world-color-red a-world)
            (color-red (get/accessor world-color-accessor a-world)))

          (define (world-color-green a-world)
            (color-green (get/accessor world-color-accessor a-world)))

          (define (world-color-blue a-world)
            (color-blue (get/accessor world-color-accessor a-world)))
          
          (define (update-world-color-red a-world a-val)
            (update/accessor (chain-accessors world-color-accessor color-red-accessor)
                             a-world
                             a-val))
          
          (define (update-world-color-green a-world a-val)
            (update/accessor (chain-accessors world-color-accessor color-green-accessor)
                             a-world
                             a-val))

          (define (update-world-color-blue a-world a-val)
            (update/accessor (chain-accessors world-color-accessor color-blue-accessor)
                             a-world
                             a-val))]

    (col (canvas (make-render-color-block world-color-accessor))
         (row "R" 
              (slider world-color-red 0 255 update-world-color-red))
         (row "G" 
              (slider world-color-green 0 255 update-world-color-green))
         (row "B" 
              (slider world-color-blue 0 255 update-world-color-blue)))))


(define main-gui
  (row (col "Color 1" (make-color-gui world-color-1-accessor))
       (col "Color 2" (make-color-gui world-color-2-accessor))
       (col "Color 3" (make-color-gui world-color-3-accessor))))


(big-bang (make-world (make-color 0 0 0) 
                      (make-color 0 0 0)
                      (make-color 0 0 0))
          main-gui)