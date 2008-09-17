#lang scheme

(require (for-syntax scheme/base
                     scheme/list
                     scheme/struct-info))

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


(define (make-drop-down default-value choices callback)
  ...)


;; random-choice: X+ -> X
;; Given a sequence of elements, chooses one of them randomly.
(define (random-choice first-elt . rest-elts)
  ...)




(define-syntax (define-replacers stx)
  (syntax-case stx ()
    [(_ a-struct-type)
     (let* ([info (extract-struct-info (syntax-local-value #'a-struct-type))]
            [fields 
             (map (lambda (accessor)
                    (datum->syntax accessor
                                   (string->symbol
                                    (substring
                                     (symbol->string (syntax-e accessor))
                                     (add1 (string-length
                                            (symbol->string
                                             (syntax-e
                                              #'a-struct-type))))))))
                  (fourth info))])
       (with-syntax ([(accessor ...) fields]
                     [(update ...) (map (lambda (id)
                                          (datum->syntax 
                                           stx
                                           (string->symbol
                                            (string-append "replace-"
                                                           (symbol->string (syntax-e #'a-struct-type))
                                                           "-"
                                                           (symbol->string (syntax-e id))))))
                                        fields)])
         (let ([result
                (syntax/loc stx
                  (begin
                    (define (update a-struct-val new-val)
                      (struct-copy a-struct-type a-struct-val
                                   (accessor new-val)))
                    ...))])
           result)))]))




(provide make-form
         make-row
         make-button
         make-drop-down
         random-choice
         
         define-replacers)