#lang scheme/base

(require scheme/contract
         "define-graph-function.ss")


(provide/contract [test-against (procedure? any/c . -> . boolean?)])




;; test-against: function/c table-function -> boolean
;; Returns true if the function passes
(define (test-against actual-function a-graph)
  (let ([gf (make-graph-function (object-name actual-function)
                                 a-graph)])
    (andmap (lambda (an-input)
              (let ([result
                     (equal? (apply actual-function an-input)
                             (apply gf an-input))])
                (when (not result)
                  ;; FIXME: integrate more closely with test engine.
                  (printf "~s doesn't match on input ~s" (object-name actual-function) an-input))
                result))
            (graph-function-inputs gf))))
  