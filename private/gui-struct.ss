#lang scheme/base

(require scheme/contract
         htdp/image
         scheme/match
         (only-in lang/htdp-beginner image?))


(define-struct elt () #:transparent)
;; An gui element is one of the following:

(define-struct (row-elt elt) (elts) #:transparent)
(define-struct (column-elt elt) (elts) #:transparent)
(define-struct (group-box-elt elt) (val-f elt enabled?-f) #:transparent)
(define-struct (string-elt elt) (val-f) #:transparent)
(define-struct (scene-elt elt) (scene-f) #:transparent)
(define-struct (button-elt elt) (val-f callback enabled?-f) #:transparent)
(define-struct (drop-down-elt elt) (val-f choices-f callback enabled?-f) #:transparent)
(define-struct (text-field-elt elt) (val-f callback enabled?-f) #:transparent)
(define-struct (slider-elt elt) (val-f min-f max-f callback enabled?-f) #:transparent)



(define world/c any/c)
(define subworld/c any/c)

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

(define (group-box val a-gui [enabled? #t])
  (make-group-box-elt (wrap-primitive string? val)
                      a-gui
                      (wrap-primitive boolean? enabled?)))




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



;; scope-struct: elt (world -> sub-world) (world sub-world -> world) -> elt
;; Scoping operator on structure.
(define (scope-struct an-elt w->s s->w)
  (match an-elt
    [(struct row-elt (elts))
     (make-row-elt (map (lambda (a-subelt) (scope-struct a-subelt w->s s->w))
                        elts))]
    [(struct column-elt (elts))
     (make-column-elt (map (lambda (a-subelt) (scope-struct a-subelt w->s s->w))
                           elts))]
    [(struct group-box-elt (val-f a-subelt enabled?-f))
     (make-group-box-elt (translate-gvalue val-f w->s)
                         (scope-struct a-subelt w->s s->w)
                         (translate-gvalue enabled?-f w->s))]
    [(struct string-elt (val-f))
     (make-string-elt (translate-gvalue val-f w->s))]
    
    [(struct scene-elt (scene-f))
     (make-scene-elt (translate-gvalue scene-f w->s))]
    
    [(struct button-elt (val-f callback enabled?-f))
     (make-button-elt (translate-gvalue val-f w->s)
                      (lambda (world) 
                        (s->w world (callback (w->s world))))
                      (translate-gvalue enabled?-f w->s))]
    
    [(struct drop-down-elt (val-f choices-f callback enabled?-f))
     (make-drop-down-elt (translate-gvalue val-f w->s)
                         (translate-gvalue choices-f w->s)
                         (translate-gcallback callback w->s s->w)
                         (translate-gvalue enabled?-f w->s))]
    
    [(struct text-field-elt (val-f callback enabled?-f))
     (make-text-field-elt (translate-gvalue val-f w->s)
                          (translate-gcallback callback w->s s->w)
                          (translate-gvalue enabled?-f w->s))]
    
    [(struct slider-elt (val-f min-f max-f callback enabled?-f))
     (make-slider-elt (translate-gvalue val-f w->s)
                      (translate-gvalue min-f w->s)
                      (translate-gvalue max-f w->s)
                      (translate-gcallback callback w->s s->w)
                      (translate-gvalue enabled?-f w->s))]))


;; translate-gvalue: (S -> X) (W -> S) -> (W -> X)
(define (translate-gvalue a-gvalue w->s)
  (lambda (a-world)
    (a-gvalue (w->s a-world))))

;; translate-gcallback: (S X -> S) (W -> S) (W S -> W) -> (W X -> W)
(define (translate-gcallback a-gcallback w->s s->w)
  (lambda (a-world a-val)
    (s->w a-world (a-gcallback (w->s a-world) a-val))))





(provide/contract [struct elt ()]
                  [struct (row-elt elt) ([elts (listof elt?)])]
                  [struct (column-elt elt) ([elts (listof elt?)])]
                  [struct (group-box-elt elt) ([val-f (gvalueof string?)]
                                               [elt elt?]
                                               [enabled?-f (gvalueof boolean?)])]
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
                  [group-box (((gvalueof* string?) elt?)
                              ((gvalueof* boolean?))
                              . ->* .
                              group-box-elt?)]
                  
                  [scope-struct (elt? 
                                 (world/c . -> . subworld/c) 
                                 (world/c subworld/c . -> . world/c)
                                 . -> . 
                                 elt?)]
                  
                  [coerse-primitive-to-elt
                   ((or/c elt? string? scene?) . -> . elt?)])