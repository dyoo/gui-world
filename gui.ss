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


;; make-button: string (world -> world) [enabled? boolean] -> element
(define (make-button label callback [enabled? #t])
  ...)


(define (make-disabled-button label)
  ...)


(define (make-drop-down default-value choices callback)
  ...)


(define (make-text-field default-value callback)
  ...)


(define (maybe-make-error msg)
  ...)



;; notify-error: world string -> world
;; For some period of time, every make-form after a notify-error will
;; Show some notification at the top of the form.
(define (notify-error world msg)
  ...)




;; random-choice: X+ -> X
;; Given a sequence of elements, chooses one of them randomly.
(define (random-choice first-elt . rest-elts)
  ...)



;; Not really a part of GUI.
;; Convenient syntax for defining all the replacing-attribute functions,
;; given a structure id.
;; Usage:
;; If we have a
;;     (define-struct posn (x y z))
;; then
;;     (define-replacers posn)
;; will expand out to definitions for replace-posn-x, replace-posn-y, replace-posn-z.
;; Each replacer takes the struct val and an attribute value, and produces a new struct val.
(define-syntax (define-updaters stx)
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
                                            (string-append "update-"
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
         make-disabled-button
         make-drop-down
         make-text-field
         maybe-make-error
         notify-error
         random-choice
         
         define-updaters)