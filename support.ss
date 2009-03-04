#lang scheme/base

(require "private/define-graph-function.ss"
         lang/prim
         lang/posn
         (prefix-in world: htdp/world))

(provide (all-from-out "private/define-graph-function.ss")
         place-image/posn)

;; Auxillary support definitions.


(define-primitive place-image/posn
  (lambda (an-image a-posn a-scene)
    (world:place-image an-image 
                       (posn-x a-posn)
                       (posn-y a-posn)
                       a-scene)))