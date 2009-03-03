#lang scheme/base

(require (for-syntax scheme/base))
(require scheme/list
         lang/posn
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
                                   (translate-output
                                    (second (first rows)))]
                                  [else
                                   (loop (rest rows))]))))

;; graph-function-inputs: graph-function -> (listof (listof input/c))
(define (graph-function-inputs a-graph-function)
  (map (lambda (a-row) (first a-row))
       (graph-function-graph a-graph-function)))



;; translate-output: X -> Y
;; Translates the output values.  If we are returning a pair of numbers,
;; we want to do so as a posn.
(define (translate-output an-output)
  (cond
    [(and (list? an-output)
          (= (length an-output) 2)
          (number? (first an-output))
          (number? (second an-output)))
     (make-posn (first an-output)
                (second an-output))]
    [else
     an-output]))
                   


;; syntax for creating graph functions that acts nicely with beginner-level scheme.
(define-syntax (define-graph-function stx)
  (syntax-case stx ()
    [(_ name a-graph)
     (identifier? #'name)
     (syntax/loc stx
       (define-primitive name (make-graph-function 'name a-graph)))]
    [(dfg args ...)
     (raise-syntax-error #f 
                         (format "Usage: (~s name-of-function a-graph)"
                                 (syntax-e #'dfg))
                         stx)]))


(provide define-graph-function
         ;; Fixme: provide proper contracts
         graph-function-inputs
         (struct-out graph-function))

