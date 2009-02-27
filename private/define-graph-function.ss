#lang scheme/base

(require (for-syntax scheme/base))
(require scheme/list
         lang/prim)

;; a row is a (list (listof input/c) output/c))


(define-struct graph-function (name  ;; symbol
                               graph ;; (listof row)
                               )
  #:transparent
  #:property prop:procedure (lambda (a-graph-function . inputs)
                              (let loop ([rows (graph-function-graph a-graph-function)])
                                (cond
                                  [(empty? rows)
                                   (error (graph-function-name a-graph-function) 
                                          "Can't be applied on inputs: ~s" 
                                          inputs)]
                                  [(equal? inputs (first (first rows)))
                                   (second (first rows))]
                                  [else
                                   (loop (rest rows))]))))

;; graph-function-inputs: graph-function -> (listof (listof input/c))
(define (graph-function-inputs a-graph-function)
  (map (lambda (a-row) (first a-row))
       (graph-function-graph a-graph-function)))


;; syntax for creating graph functions that acts nicely with beginner-level scheme.
(define-syntax (define-graph-function stx)
  (syntax-case stx ()
    [(_ name a-graph)
     (identifier? #'name)
     (syntax/loc stx
       (define-primitive name (make-graph-function 'name a-graph)))]
    [else
     (raise-syntax-error #f "Usage: (define-graph-function name-of-function a-graph)"
                         stx)]))


(provide define-graph-function
         ;; Fixme: provide proper contracts
         graph-function-inputs
         (struct-out graph-function))

