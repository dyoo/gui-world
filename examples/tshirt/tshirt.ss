#lang scheme/base
(require "../../gui-world.ss"
         "fabric-teachpack.scm")

;; tshirt gui program.  Allows users to design their own tshirts.
(define-struct world (color width fabric))
(define-updaters world)

;; COLORS: (listof string)
;; The colors we'll provide.
(define COLORS (list "red" "white" "blue" "green" "orange" "black"))

;; The initial state of the world will be to select the black color, with a 
;; stripe length of five, and the empty fabric.
(define initial-world 
  (make-world "black"
              5
              (create-solid-fabric "white" 
                                   (image-width tshirt)
                                   (image-height tshirt))))

;; draw-shirt: world -> scene
;; Draw a shirt with the current world's fabric.
(define (draw-shirt a-world)
  (place-image (overlay (world-fabric a-world) tshirt)
               (/ (image-width tshirt) 2)
               (/ (image-height tshirt) 2)
               (empty-scene (image-width tshirt) (image-height tshirt))))

;; world-add-horizontal: world -> world
(define (world-add-horizontal a-world)
  (update-world-fabric a-world
                       (add-horiz-stripe (world-color a-world)
                                         (world-width a-world)
                                         (world-fabric a-world))))

;; world-add-vertical: world -> world
(define (world-add-vertical a-world) 
  (update-world-fabric a-world
                       (add-vertical-stripe (world-color a-world)
                                            (world-width a-world)
                                            (world-fabric a-world))))

;; Our gui will consist of the tshirt's display and controls to add more stripes.
(define a-gui
  (col (scene draw-shirt)
       (slider world-width 1 20 update-world-width)
       (drop-down world-color COLORS update-world-color)
       (row (button "Add horizontal stripe" world-add-horizontal)
            (button "Add vertical stripe" world-add-vertical))))

(big-bang initial-world a-gui)