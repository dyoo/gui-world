#lang scheme
(provide TODO)

(define-syntax (TODO stx)
  (syntax-case stx ()
    [_
     (identifier? stx)
     (syntax/loc stx 
       (error 'TODO "Not done yet."))]))