#lang scheme/base
(require scheme/contract
         scheme/list)


(define function/c (any/c . -> . any/c))

(provide/contract [test-against (function/c any/c . -> . boolean?)])


;; input=?: any any -> boolean
(define (input=? x y)
  (equal? x y))


(define (apply-table-function a-table an-input)
  (let loop ([rows a-table])
    (cond
      [(empty? rows)
       (error 'table-function "Can't apply ~s" an-input)]
      [(equal? an-input (first (first rows)))
       (second (first rows))]
      [else
       (loop (rest rows))])))


(define (table-function-inputs a-table-function)
  (map (lambda (a-row) (first a-row))
       a-table-function))


;; test-against: function/c table-function -> boolean
;; Returns true if the function passes
(define (test-against actual-function a-table-function)
  (andmap (lambda (an-input)
            (input=? (apply actual-function an-input)
                     (apply-table-function a-table-function an-input)))
          (table-function-inputs a-table-function)))
