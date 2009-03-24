#lang scheme/base

(require (for-syntax scheme/base))
(require scheme/list
         lang/posn
         scheme/match
         lang/prim
         scheme/sandbox)

;; a row is a (list (listof input/c) output/c))



(define-struct tabular-graph-function (name  ;; symbol
                                       graph ;; (listof row)
                                       )
  #:transparent
  #:property prop:procedure 
  (lambda (a-graph-function . inputs)
    (let loop ([rows (tabular-graph-function-graph a-graph-function)])
      (cond
        [(empty? rows)
         (error (tabular-graph-function-name a-graph-function) 
                "Can't be applied on inputs: ~s" 
                inputs)]
        [(equal? (map translate-input inputs)
                 (map translate-input (first (first rows))))
         (translate-output
          (second (first rows)))]
        [else
         (loop (rest rows))]))))
  
  

(define-struct lambda-graph-function (name a-lambda-sexp lambda)
  #:transparent
  #:property prop:procedure
  (lambda (a-graph-function . inputs)
    (apply (lambda-graph-function-lambda a-graph-function) inputs)))



;; graph-function-inputs: graph-function -> (listof (listof input/c))
#;(define (graph-function-inputs a-graph-function)
    (map (lambda (a-row) (first a-row))
         (graph-function-graph a-graph-function)))


;; translate-input: X -> Y
(define (translate-input an-input)
  (cond
    [(posn? an-input)
     (list (posn-x an-input)
           (posn-y an-input))]
    [else
     an-input]))


;; translate-output: X -> Y
;; Translates the output values.
;; If we are returning a pair of numbers,
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
       (begin
         (define -implementation  
           (choose-graph-function-implementation 'name 'a-graph))
         (define-primitive name -implementation)))]
    [(dfg args ...)
     (raise-syntax-error #f 
                         (format "Usage: (~s name-of-function a-graph)"
                                 (syntax-e #'dfg))
                         stx)]))



(define (choose-graph-function-implementation name a-graph)
  (cond
    [(lambda-graph? a-graph)
     (make-lambda-graph-function name 
                                 a-graph 
                                 (parameterize 
                                     ([sandbox-eval-limits (list #f #f)]
                                      [sandbox-eval-handlers (list #f #f)]
                                      [sandbox-memory-limit #f]
                                      [sandbox-namespace-specs
                                       (let ([specs (sandbox-namespace-specs)])
                                         `(,(car specs)
                                           ,@(cdr specs)
                                           lang/posn
                                           ,@(if gui? '(mrlib/cache-image-snip) '())))])
                                   (let ([my-eval 
                                          (make-evaluator 
                                           'scheme/base
                                           #:requires (list 'lang/posn))])
                                     (let ([result
                                            (my-eval a-graph)])
                                       result))))]
    [else
     (make-tabular-graph-function name (second a-graph))]))


(define (lambda-graph? a-graph)
  (match a-graph
    [(list 'lambda (list args ...) body)
     #t]
    [else
     #f]))

      
(provide define-graph-function
         choose-graph-function-implementation
         ;; Fixme: provide proper contracts
         #; graph-function-inputs
         #; (struct-out graph-function))

