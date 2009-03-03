#lang scheme/base

(require scheme/match
         "graph-explorer.ss")


(define (world->syntax a-world)
  (datum->syntax #f
                 (world-function-as-sexpression a-world)))



(define (world->bytes a-world)
  
  ...)


(define (bytes->world a-world)
  ...)


;; world->thumbnail: world -> scene
(define (world->thumbnail a-world)
  (world-plot a-world))



(provide initial-world 
         view 
         world->syntax 
         world->bytes 
         bytes->world
         world->thumbnail)