#lang scheme/base
(require scheme/contract
         "struct.ss")


(define function/c (any/c . -> . any/c))

(provide/contract [test-against (function/c partial-function? . -> . boolean?)])


;; input=?: any any -> boolean
(define (input=? x y)
  (equal? x y))

;; test-against: function/c partial-function -> boolean
;; Returns true if the function passes
(define (test-against actual-function expected-function)
  (andmap (lambda (an-input)
            (input=? (actual-function an-input)
                     (expected-function an-input)))))