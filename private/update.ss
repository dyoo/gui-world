#lang scheme/base
(require htdp/image
         lang/posn
         scheme/stxparam
         lang/prim
         "accessor.ss"
         (prefix-in beginner: lang/htdp-beginner)
         (for-syntax scheme/base
                     scheme/struct-info
                     scheme/list
                     syntax/boundmap
                     scheme/stxparam))


(begin-for-syntax
  (define getter-updater-mappings (make-free-identifier-mapping)))

(define-syntax-parameter scoped-getter-updater-mappings '())


;; register-getter-updater: syntax syntax -> void
;; Attaches a binding between a getter and its updater.
(define-for-syntax (register-getter-updater getter updater)
  ;; fixme: record more information.
  (free-identifier-mapping-put! getter-updater-mappings 
                                getter 
                                updater))


;; lookup-updater/getter: syntax -> (or syntax #f)
;; Tries to find the updater for a given getter, or #f if we can't
;; find it.
(define-for-syntax (lookup-updater/getter getter)
  (let loop ([local-getter-updater-bindings
              (syntax-parameter-value #'scoped-getter-updater-mappings)])
    (cond
      [(empty? local-getter-updater-bindings)
       (free-identifier-mapping-get getter-updater-mappings
                                    getter
                                    (lambda ()
                                      #f))]
      [(free-identifier=? 
        (first (first local-getter-updater-bindings))
                          getter)
       (second (first local-getter-updater-bindings))]
      [else
       (loop (rest local-getter-updater-bindings))])))


;; with-getter/updater: syntax
;; Locally bind a getter/updater pair so that update can recognize it.
(define-syntax (with-getter/updater stx)
    (syntax-case stx ()
      [(_ [(getter updater) ...] body ...)
       (syntax/loc stx
         (syntax-parameterize 
          ([scoped-getter-updater-mappings
            (list* (list #'getter #'updater) ...
                   (syntax-parameter-value 
                    #'scoped-getter-updater-mappings))])
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
              [descriptor (first info)]
              [predicate (third info)]
              [getters (fourth info)]
              [fields 
               (map (lambda (getter)
                      (datum->syntax getter
                                     (string->symbol
                                      (substring
                                       (symbol->string (syntax-e getter))
                                       (add1 (string-length
                                              (symbol->string
                                               (syntax-e
                                                #'a-struct-type))))))))
                    getters)]
              [make-update-stx (lambda (id)
                                 (datum->syntax 
                                  stx
                                  (string->symbol
                                   (string-append "update-"
                                                  (symbol->string (syntax-e #'a-struct-type))
                                                  "-"
                                                  (symbol->string (syntax-e id))))))]
              [make-accessor-stx (lambda (id)
                                 (datum->syntax 
                                  stx
                                  (string->symbol
                                   (string-append (symbol->string (syntax-e #'a-struct-type))
                                                  "-"
                                                  (symbol->string (syntax-e id))
                                                  "-accessor"))))])
         (with-syntax ([(field ...) fields]
                       [descriptor descriptor]
                       [predicate predicate]
                       [(getter ...) getters]
                       [(-updater ...) (generate-temporaries fields)]
                       [(updater ...) (map make-update-stx fields)]
                       [(accessor ...) (map make-accessor-stx fields)])
           (let ([result
                  (syntax/loc stx
                    (begin
                      
                      ;; workaround for beginner-level weirdness with expansions from the toplevel.
                      ;; see: http://list.cs.brown.edu/pipermail/plt-scheme/2008-October/027974.html
                      ;; Issue might be fixed in DrScheme 4.1.3.
                      (let-syntax ([do-compile-time-registration 
                                    (lambda (stx)
                                      ;; mark the accessors for the updater to cooperate
                                      (register-getter-updater #'getter #'updater) ...
                                      (syntax (void)))])
                        (do-compile-time-registration))
                      
                      (define (-updater a-struct-val new-val)
                        (unless (predicate a-struct-val)
                          (raise (make-exn:fail
                                  (format "~s: expects argument of type <~s>, but got ~s instead"
                                          'updater
                                          'descriptor 
                                          a-struct-val)
                                  (current-continuation-marks))))
                        (struct-copy a-struct-type a-struct-val
                                     (field new-val)))
                      ...
                      (define-primitive updater -updater) ...
                      (define accessor (make-accessor (lambda (x) (getter x)) -updater)) ...
                      ))])
             result))))]))



;; fixme
(define-for-syntax (getter? stx)
  (and (lookup-updater/getter stx)
       #t))


(define-syntax (update stx)
  (syntax-case stx ()
    [(update (outer-getter (inner-getter rest-inner ...)) val)
     (and (getter? #'outer-getter)
          (getter? #'inner-getter))
     (with-syntax ([a-mutator-outer (lookup-updater/getter #'outer-getter)])
       (syntax/loc stx
         (update (inner-getter rest-inner ...)
                 (a-mutator-outer (inner-getter rest-inner ...) val))))]
    [(update (a-getter datum) val)
     (getter? #'a-getter)
     (with-syntax ([a-mutator (lookup-updater/getter #'a-getter)])
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

;; Posns
(define (update-posn-x a-posn val)
  (make-posn val (posn-y a-posn)))

(define (update-posn-y a-posn val)
  (make-posn (posn-x a-posn) val))

(begin-for-syntax
  (register-getter-updater #'color-red #'update-color-red)
  (register-getter-updater #'beginner:color-red #'update-color-red)
  (register-getter-updater #'color-green #'update-color-green)
  (register-getter-updater #'beginner:color-green #'update-color-green)
  (register-getter-updater #'color-blue #'update-color-blue)
  (register-getter-updater #'beginner:color-blue #'update-color-blue)
  (register-getter-updater #'posn-x #'update-posn-x)
  (register-getter-updater #'beginner:posn-x #'update-posn-x)
  (register-getter-updater #'posn-y #'update-posn-y)
  (register-getter-updater #'beginner:posn-y #'update-posn-y))


(define color-red-accessor (make-accessor color-red update-color-red))
(define color-green-accessor (make-accessor color-green update-color-green))
(define color-blue-accessor (make-accessor color-blue update-color-blue))
(define posn-x-accessor (make-accessor posn-x update-posn-x))
(define posn-y-accessor (make-accessor posn-y update-posn-y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide update define-updaters with-getter/updater
         update-color-red update-color-green update-color-blue
         update-posn-x update-posn-y
         
         color-red-accessor color-green-accessor color-blue-accessor
         posn-x-accessor posn-y-accessor)