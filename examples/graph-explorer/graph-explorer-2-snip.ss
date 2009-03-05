#lang scheme/base

(require scheme/match
         "../../gui-world.ss"
         "graph-explorer-2.ss")

;; world->syntax: world -> syntax
(define (world->syntax a-world)
  (datum->syntax #f (world-function-as-lambda a-world)))


;; world->bytes: world -> bytes
(define (world->bytes a-world)
  (let ([op (open-output-bytes)])
    (match a-world
      [(struct world (name args x-name x-args x-body y-name y-args y-body plot x-plot y-plot dirty?))
       (write (list name args x-name x-args x-body y-name y-args y-body dirty?) op)
       (get-output-bytes op)])))


;; bytes->world: bytes -> world
(define (bytes->world some-bytes)
  (let ([ip (open-input-bytes some-bytes)])
    (match (read ip)
      [(list name args x-name x-args x-body y-name y-args y-body dirty?)
       (world-replot
        (make-world name args x-name x-args x-body y-name y-args y-body 
                    (empty-scene WIDTH HEIGHT)
                    (empty-scene WIDTH HEIGHT)
                    (empty-scene WIDTH HEIGHT)
                    dirty?))])))


;; world->thumbnail: world -> scene
(define (world->thumbnail a-world)
  (world-plot a-world))

(provide initial-world 
         view 
         world->syntax 
         world->bytes 
         bytes->world
         world->thumbnail)