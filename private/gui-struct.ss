#lang scheme/base

(require scheme/contract
         htdp/image
         (only-in lang/htdp-beginner image?))


(define-struct elt () #:transparent)
;; An gui element is one of the following:

(define-struct (row-elt elt) (elts) #:transparent)
(define-struct (column-elt elt) (elts) #:transparent)
(define-struct (string-elt elt) (val-f) #:transparent)
(define-struct (scene-elt elt) (scene-f) #:transparent)
(define-struct (button-elt elt) (val-f callback enabled?-f) #:transparent)
(define-struct (drop-down-elt elt) (val-f choices-f callback enabled?-f) #:transparent)
(define-struct (text-field-elt elt) (val-f callback enabled?-f) #:transparent)
(define-struct (slider-elt elt) (val-f min-f max-f callback enabled?-f) #:transparent)



(define world/c any/c)

(define (gvalueof t)
  (world/c . -> . t))

(define (gvalueof* t)
  (or/c (world/c . -> . t)
        t))

(define (gcallbackof t)
  (world/c t . -> . world/c))

(define callback/c
  (world/c . -> . world/c))


;; scene?: any -> boolean
(define (scene? i)
  (and (image? i)
       (= 0 (pinhole-x i))
       (= 0 (pinhole-y i))))


(define (wrap-primitive a-pred a-gvalue)
  (cond
    [(a-pred a-gvalue)
     (lambda (a-world) a-gvalue)]
    [(procedure? a-gvalue)
     a-gvalue]
    [else 
     (error 'wrap-primitive)]))


(define (message a-gvalue)
  (make-string-elt (wrap-primitive string? a-gvalue)))

(define (button val callback [enabled #t])
  (make-button-elt (wrap-primitive string? val)
                   callback
                   (wrap-primitive boolean? enabled)))

(define (slider val min max callback [enabled? #t])
  (make-slider-elt (wrap-primitive number? val)
                   (wrap-primitive number? min)
                   (wrap-primitive number? max)
                   callback
                   (wrap-primitive boolean? enabled?)))

(define (drop-down val choices callback [enabled? #t])
  (make-drop-down-elt (wrap-primitive string? val)
                      (wrap-primitive (flat-contract-predicate (listof string?))
                                      choices)
                      callback
                      (wrap-primitive boolean? enabled?)))

(define (text-field val callback [enabled? #t])
  (make-text-field-elt (wrap-primitive string? val)
                       callback
                       (wrap-primitive boolean? enabled?)))

(define (scene a-scene)
  (make-scene-elt (wrap-primitive scene? a-scene)))


(define (row . elts)
  (make-row-elt (coerse-primitive-types-to-elts elts)))

(define (col . elts)
  (make-column-elt (coerse-primitive-types-to-elts elts)))


;; coerse-primitive-types-to-elts: (listof (or/c elt string scene)) -> (listof elt)
;; Helper to turn strings into string-elts, and images into image-elts.
(define (coerse-primitive-types-to-elts elts)
  (map coerse-primitive-to-elt elts))

;; coerse-primitive-type-to-elt: (or/c elt string scene) -> elt
(define (coerse-primitive-to-elt an-elt)
  (cond [(string? an-elt)
         (message an-elt)]
        [(scene? an-elt)
         (scene an-elt)]
        [else
         an-elt]))

       
(provide/contract [struct elt ()]
                  [struct (row-elt elt) ([elts (listof elt?)])]
                  [struct (column-elt elt) ([elts (listof elt?)])]
                  [struct (string-elt elt) ([val-f (gvalueof string?)])]
                  [struct (scene-elt elt) ([scene-f (gvalueof scene?)])]
                  [struct (button-elt elt) ([val-f (gvalueof string?)]
                                            [callback callback/c]
                                            [enabled?-f (gvalueof boolean?)])]
                  [struct (drop-down-elt elt) ([val-f (gvalueof string?)]
                                           [choices-f (gvalueof (listof string?))]
                                           [callback (gcallbackof string?)]
                                           [enabled?-f (gvalueof boolean?)])]
                  [struct (text-field-elt elt) ([val-f (gvalueof string?)]
                                           [callback (gcallbackof string?)]
                                           [enabled?-f (gvalueof boolean?)])]
                  [struct (slider-elt elt) ([val-f (gvalueof number?)]
                                            [min-f (gvalueof number?)]
                                            [max-f (gvalueof number?)]
                                            [callback (gcallbackof number?)]
                                            [enabled?-f (gvalueof boolean?)])]

                  
                  
                  [message ((gvalueof* string?) . -> . string-elt?)]
                  
                  [button (((gvalueof* string?) 
                            callback/c)
                           ((gvalueof* boolean?)) 
                           . ->* . button-elt?)]
                  
                  [slider (((gvalueof* number?) 
                            (gvalueof* number?)
                            (gvalueof* number?)
                            (gcallbackof number?))
                           ((gvalueof boolean?))
                           . ->* . slider-elt?)]
                  
                  [drop-down (((gvalueof* string?)
                               (gvalueof* (listof string?))
                               (gcallbackof string?)) 
                              ((gvalueof boolean?))
                              . ->* . drop-down-elt?)]
                  
                  [text-field (((gvalueof* string?)
                               (gcallbackof string?))
                               ((gvalueof boolean?))
                               . ->* . text-field-elt?)]

                  [scene ((gvalueof* scene?) . -> . scene-elt?)]
                  
                  [row (() () #:rest (listof (or/c elt? string? scene?)) . ->* . row-elt?)]
                  [col (() () #:rest (listof (or/c elt? string? scene?)) . ->* . column-elt?)]

                  
                  [coerse-primitive-to-elt
                   ((or/c elt? string? scene?) . -> . elt?)]
                  )