#lang scheme/base

(require (for-syntax scheme/base))
(require scheme/list
         lang/prim)

(define (get-graph-function a-graph)
  (lambda inputs
    (let loop ([rows a-graph])
      (cond
        [(empty? rows)
         (error 'table-function "Can't apply ~s" inputs)]
        [(equal? inputs (first (first rows)))
         (second (first rows))]
        [else
         (loop (rest rows))]))))


(define (graph-function-inputs a-graph-function)
  (map (lambda (a-row) (first a-row))
       a-graph-function))


(define-syntax (define-graph-function stx)
  (syntax-case stx ()
    [(_ name a-graph)
     (identifier? #'name)
     (syntax/loc stx
       (define-primitive name (get-graph-function a-graph)))]
    [else
     (raise-syntax-error #f "Usage: (define-graph-function name-of-function a-graph)"
                         stx)]))


(provide define-graph-function get-graph-function)