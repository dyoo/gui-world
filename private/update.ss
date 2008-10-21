#lang scheme/base
(require htdp/image
         lang/posn
         scheme/stxparam
         lang/prim
         (for-syntax scheme/base
                     scheme/struct-info
                     scheme/list
                     syntax/boundmap
                     scheme/stxparam))

(begin-for-syntax
  (define accessor-updater-mappings (make-free-identifier-mapping)))

(define-syntax-parameter scoped-accessor-updater-mappings '())


;; register-accessor-updater: syntax syntax -> void
;; Attaches a binding between an accessor and its updater.
(define-for-syntax (register-accessor-updater accessor updater)
  ;; fixme: record more information.
  (free-identifier-mapping-put! accessor-updater-mappings 
                                accessor 
                                updater))


;; lookup-accessor-updater: syntax -> (or syntax #f)
;; Tries to find the updater for a given accessor, or #f if we can't
;; find it.
(define-for-syntax (lookup-accessor-updater accessor)
  (let loop ([local-accessor-updater-bindings
              (syntax-parameter-value #'scoped-accessor-updater-mappings)])
    (cond
      [(empty? local-accessor-updater-bindings)
       (free-identifier-mapping-get accessor-updater-mappings
                                    accessor
                                    (lambda () #f))]
      [(free-identifier=? (first (first local-accessor-updater-bindings))
                           accessor)
       (second (first local-accessor-updater-bindings))]
      [else
       (loop (rest local-accessor-updater-bindings))])))


;; with-accessor/updater: syntax
;; Locally bind an accessor/updater pair so that update can recognize it.
(define-syntax (with-accessor/updater stx)
  (syntax-case stx ()
    [(_ [(accessor updater) ...] body ...)
     (syntax/loc stx
       (syntax-parameterize 
        ([scoped-accessor-updater-mappings
          (list* (list #'accessor #'updater) ...
                 (syntax-parameter-value 
                  #'scoped-accessor-updater-mappings))])
        body ...))]))




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
                     [(-updater ...) (generate-temporaries fields)]
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
                    (let-syntax ([do-compile-time-registration (lambda (stx)
                      ;; mark the accessors for the updater to cooperate
                                        (register-accessor-updater #'accessor #'updater) ...
                                        (syntax (void)))])
                      (do-compile-time-registration))
                    (define (-updater a-struct-val new-val)
                      (struct-copy a-struct-type a-struct-val
                                   (field new-val)))
                    ...
                    (define-primitive updater -updater) ...
                    
                                                     ))])
           result))))]))



;; mutator: syntax -> syntax
;; Gets the mutator for the given accessor.
(define-for-syntax (mutator accessor)
  (lookup-accessor-updater accessor))


;; fixme
(define-for-syntax (accessor? stx)
  (and (lookup-accessor-updater stx)
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
;; update-color-red: color number -> color
(define (update-color-red a-color val)
  (make-color val
              (color-green a-color)
              (color-blue a-color)))

;; update-color-green: color number -> color
(define (update-color-green a-color val)
  (make-color (color-red a-color)
              val
              (color-blue a-color)))

;; update-color-blue: color number -> color
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


(provide update define-updaters with-accessor/updater
         update-color-red update-color-green update-color-blue
         update-posn-x update-posn-y)