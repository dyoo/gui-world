#lang scheme/base

(require scheme/list
         scheme/contract)

(define-struct io (input output) 
  #:transparent)

(define-struct partial-function (ios)
  #:transparent
  #:property prop:procedure (lambda (pf an-input)
                              (let loop ([ios (partial-function-ios pf)])
                                (cond
                                  [(empty? ios)
                                   (error 'partial-function "Cannot apply to input ~s" an-input)]
                                  [(equal? an-input (io-input (first ios)))
                                   (io-output (first ios))]
                                  [else
                                   (loop (rest ios))]))))



(provide/contract [struct io ([input any/c]
                              [output any/c])]
                  [struct partial-function ([ios (listof io?)])])