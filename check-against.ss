#lang scheme/base
(require "private/test-against.ss"
         lang/prim
         test-engine/scheme-tests)
(require (for-syntax scheme/base))


(define-syntax (check-against stx)
  (syntax-case stx ()
    [(_ a-function-id a-table-expr)
     (syntax/loc stx
       (check-expect (test-against (first-order->higher-order a-function-id)
                                   a-table-expr) #t))]))

(provide check-against)

