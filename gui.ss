#lang scheme


(define ... 'fixme)

;; make-form: element+ -> form
(define (make-form first-elt . rest-elements)
  ...)


;; make-row: element+ -> element
(define (make-row first-elt . rest-elt)
  ...)


;; make-button: string (world -> world) -> element
(define (make-button label callback)
  ...)





(provide make-form
         make-row
         make-button)