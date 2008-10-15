#lang scheme/base
(require (for-syntax scheme/base
                     scheme/struct-info
                     scheme/list
                     syntax/boundmap))

(begin-for-syntax
  (define mappings (make-free-identifier-mapping)))

(define-for-syntax (register struct-type accessor updater)
  (free-identifier-mapping-put! mappings accessor struct-type))

(define-syntax (define-updaters stx)
  (syntax-case stx ()
    [(_ a-struct-type)
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
                    (begin-for-syntax
                      ;; mark the accessors for the updater to cooperate
                      (register #'a-struct-type #'accessor #'update) ...)
                    (define (update a-struct-val new-val)
                      (struct-copy a-struct-type a-struct-val
                                   (field new-val)))
                    ...))])
           result)))]))


(define-struct world (state) #:transparent)
(define-struct state (color) #:transparent)
(define-struct color (r g b) #:transparent)
(define-updaters world)
(define-updaters state)
(define-updaters color)



;; mutator: syntax -> syntax
;; Gets the mutator for the given accessor.
(define-for-syntax (mutator accessor)
  (datum->syntax
   accessor
   (string->symbol
    (string-append "update-" 
                   (symbol->string (syntax-e accessor))))))
   

;; fixme
(define-for-syntax (accessor? stx)
  (and (free-identifier-mapping-get mappings stx (lambda () #f))
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