#lang scheme/base
(require htdp/image
         lang/posn
         (for-syntax scheme/base
                     scheme/struct-info
                     scheme/list
                     syntax/boundmap))

(begin-for-syntax
  (define accessor-updater-mappings (make-free-identifier-mapping)))


;; register-accessor-updater
(define-for-syntax (register-accessor-updater accessor updater)
  ;; fixme: record more information.
  (free-identifier-mapping-put! accessor-updater-mappings accessor updater))


(define-for-syntax (check-struct-type! a-struct-type ctx-stx)
  ;; Check that a-struct-type is really a struct type.
  (unless (struct-info? 
           (syntax-local-value a-struct-type (lambda () #f)))
    (raise-syntax-error #f "Unknown structure type" ctx-stx 
                        a-struct-type)))


(define-syntax (define-updaters stx)
  (syntax-case stx ()
    [(_ a-struct-type)
     (begin
       (check-struct-type! #'a-struct-type stx)
       (let* ([info (extract-struct-info (syntax-local-value #'a-struct-type))]
            [accessors (fourth info)]
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
                  accessors)])
       (with-syntax ([(field ...) fields]
                     [(accessor ...) accessors]
                     [(updater ...) (map (lambda (id)
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
                    (begin-for-syntax
                      ;; mark the accessors for the updater to cooperate
                      (register-accessor-updater #'accessor #'updater) ...)
                    (define (updater a-struct-val new-val)
                      (struct-copy a-struct-type a-struct-val
                                   (field new-val)))
                    ...))])
           result))))]))



;; mutator: syntax -> syntax
;; Gets the mutator for the given accessor.
(define-for-syntax (mutator accessor)
  (free-identifier-mapping-get accessor-updater-mappings accessor)
  #;(datum->syntax
     accessor
     (string->symbol
      (string-append "update-" 
                     (symbol->string (syntax-e accessor))))))


;; fixme
(define-for-syntax (accessor? stx)
  (and (free-identifier-mapping-get accessor-updater-mappings stx 
                                    (lambda () #f))
       #t))


(define-syntax (update stx)
  (syntax-case stx ()
    [(update (accessor-outer (accessor-inner rest-inner ...)) val)
     (and (accessor? #'accessor-outer)
          (accessor? #'accessor-inner))
     (with-syntax ([a-mutator-outer (mutator #'accessor-outer)])
       (syntax/loc stx
         (update (accessor-inner rest-inner ...)
                 (a-mutator-outer (accessor-inner rest-inner ...) val))))]
    [(update (accessor datum) val)
     (accessor? #'accessor)
     (with-syntax ([a-mutator (mutator #'accessor)])
       (syntax/loc stx
         (a-mutator datum val)))]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hack: we'll need to register updaters for the built-in structures.

;; Colors
(define (update-color-red a-color val)
  (make-color val
              (color-green a-color)
              (color-blue a-color)))

(define (update-color-green a-color val)
  (make-color (color-red a-color)
              val
              (color-blue a-color)))

(define (update-color-blue a-color val)
  (make-color (color-red a-color)
              (color-green a-color)
              val))
(begin-for-syntax
  (register-accessor-updater #'color-red #'update-color-red)
  (register-accessor-updater #'color-green #'update-color-green)
  (register-accessor-updater #'color-blue #'update-color-blue))


;; Posns
(define-updaters posn)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide update define-updaters
         
         update-color-red update-color-green update-color-blue
         update-posn-x update-posn-y)