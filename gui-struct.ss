#lang scheme/base

(require scheme/contract
         scheme/class
         htdp/image
         mrlib/cache-image-snip
         (only-in lang/htdp-beginner image?))


(define-struct elt () #:transparent)
;; An gui element is one of the following:

(define-struct (row-elt elt) (elts) #:transparent)
(define-struct (column-elt elt) (elts) #:transparent)
(define-struct (string-elt elt) (val) #:transparent)
(define-struct (button-elt elt) (val callback enabled?) #:transparent)
(define-struct (drop-down-elt elt) (val choices callback) #:transparent)
(define-struct (text-field-elt elt) (val callback) #:transparent)
(define-struct (slider-elt elt) (val min max callback) #:transparent)
(define-struct (scene-elt elt) (scene) #:transparent)


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

(define (slider val min max callback)
  (make-slider-elt (wrap-primitive number? val)
                   (wrap-primitive number? min)
                   (wrap-primitive number? max)
                   callback))

(define (drop-down val choices callback)
  (make-drop-down-elt (wrap-primitive string? val)
                      (wrap-primitive (flat-contract-predicate (listof string?))
                                      choices)
                      callback))

(define (text-field val callback)
  (make-text-field-elt (wrap-primitive string? val)
                       callback))

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

       
(provide/contract [struct (row-elt elt) ([elts (listof elt?)])]
                  [struct (column-elt elt) ([elts (listof elt?)])]
                  [struct (string-elt elt) ([val (gvalueof string?)])]
                  [struct (button-elt elt) ([val (gvalueof string?)]
                                            [callback callback/c]
                                            [enabled? (gvalueof boolean?)])]
                  [struct (drop-down-elt elt) ([val (gvalueof string?)]
                                           [choices (gvalueof (listof string?))]
                                           [callback (gcallbackof string?)])]
                  [struct (text-field-elt elt) ([val (gvalueof string?)]
                                           [callback (gcallbackof string?)])]
                  [struct (slider-elt elt) ([val (gvalueof number?)]
                                            [min (gvalueof number?)]
                                            [max (gvalueof number?)]
                                            [callback (gcallbackof number?)])]
                  [struct (scene-elt elt) ([scene (gvalueof scene?)])]
                  
                  
                  [message ((gvalueof* string?) . -> . string-elt?)]

                  [button ((gvalueof* string?) 
                           callback/c 
                           (gvalueof* boolean?) . -> . button-elt?)]
                  
                  [slider ((gvalueof* number?) 
                           (gvalueof* number?)
                           (gvalueof* number?)
                           (gcallbackof number?) . -> . slider-elt?)]
                  
                  [drop-down ((gvalueof* string?)
                              (gvalueof* (listof string?))
                              (gcallbackof string?) . -> . drop-down-elt?)]
                  
                  [text-field ((gvalueof* string?)
                               (gcallbackof string?) . -> . text-field-elt?)]

                  [scene ((gvalueof* scene?) . -> . scene-elt?)]
                  
                  [row (() (listof (or/c elt? string? scene?)) . ->* . row-elt?)]
                  [col (() (listof (or/c elt? string? scene?)) . ->* . column-elt?)]

                  
                  [coerse-primitive-to-elt
                   ((or/c elt? string? scene?) . -> . elt?)]
                  )