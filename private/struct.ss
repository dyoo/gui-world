#lang scheme/base

(require scheme/list
         scheme/contract)

(define-struct io (input output) 
  #:transparent)

;; update-io-input: io any -> io
(define (update-io-input an-io an-input)
  (make-io an-input (io-output an-io)))


;; update-io-output: io any -> io
(define (update-io-output an-io an-output)
  (make-io (io-input an-io) an-output))


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
                  [update-io-input (io? any/c . -> . io?)]
                  [update-io-output (io? any/c . -> . io?)]
                  [struct partial-function ([ios (listof io?)])])