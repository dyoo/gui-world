#lang scheme/base

(require scheme/contract
         scheme/list
         test-engine/scheme-tests)



(provide/contract [test-against (procedure? any/c . -> . boolean?)])




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
            (let ([result
                   (equal? (apply actual-function an-input)
                           (apply-table-function a-table-function an-input))])
              (when (not result)
                ;; FIXME: integrate more closely with test engine.
                (printf "~s doesn't match on input ~s" (object-name actual-function) an-input))
              result))
          (table-function-inputs a-table-function)))
