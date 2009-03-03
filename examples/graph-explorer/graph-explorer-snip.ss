#lang scheme/base

(require scheme/match
         "../../gui-world.ss"
         "graph-explorer.ss")

;; world->syntax: world -> syntax
(define (world->syntax a-world)
  (datum->syntax #f (world-function-as-sexpression a-world)))


;; world->bytes: world -> bytes
(define (world->bytes a-world)
  (let ([op (open-output-bytes)])
    (match a-world
      [(struct world (name args body plot dirty?))
       (write (list name args body dirty?) op)
       (get-output-bytes op)])))


;; bytes->world: bytes -> world
(define (bytes->world some-bytes)
  (let ([ip (open-input-bytes some-bytes)])
    (match (read ip)
      [(list name args body dirty?)
       (world-replot
        (make-world name args body (empty-scene WIDTH HEIGHT) dirty?))])))


;; world->thumbnail: world -> scene
(define (world->thumbnail a-world)
  (world-plot a-world))

(provide initial-world 
         view 
         world->syntax 
         world->bytes 
         bytes->world
         world->thumbnail)