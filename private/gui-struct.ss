#lang scheme/base

(require scheme/contract
         htdp/image
         scheme/match
         "prim.ss"
         (only-in lang/htdp-beginner image?))


(define-struct elt () #:transparent)
;; An gui element is one of the following:

(define-struct (row-elt elt) (elts) #:transparent)
(define-struct (column-elt elt) (elts) #:transparent)
(define-struct (box-group-elt elt) (val-f elt enabled?-f) #:transparent)
(define-struct (string-elt elt) (val-f) #:transparent)
(define-struct (canvas-elt elt) (scene-f callback) #:transparent)
(define-struct (button-elt elt) (val-f callback enabled?-f) #:transparent)
(define-struct (drop-down-elt elt) (val-f choices-f callback enabled?-f) #:transparent)
(define-struct (text-field-elt elt) (val-f callback enabled?-f) #:transparent)
(define-struct (slider-elt elt) (val-f min-f max-f callback enabled?-f) #:transparent)
(define-struct (checkbox-elt elt) (val-f callback enabled?-f) #:transparent)



(define world/c any/c)
(define subworld/c any/c)

(define (gvalueof t)
  (world/c . -> . t))

(define (gvalueof* t)
  (or/c (world/c . -> . t)
        t))

(define (gcallbackof t)
  (world/c t . -> . world/c))

(define (gcallbackof-2 t1 t2)
  (world/c t1 t2 . -> . world/c))

(define callback/c
  (world/c . -> . world/c))


;; displayable?: any -> boolean
(define (displayable? datum)
  (or (string? datum)
      (number? datum)))


;; displayable->string: displayable -> string
(define (displayable->string datum)
  (match datum 
    [(? string?)
     datum]
    [(? number?) 
     (number->string datum)]))



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



(define (row . elts)
  (make-row-elt (coerse-primitive-types-to-elts elts)))

(define (col . elts)
  (make-column-elt (coerse-primitive-types-to-elts elts)))


(define (message a-gvalue)
  (make-string-elt (wrap-primitive string? a-gvalue)))


(define (button/enabled val callback [enabled #t])
  (make-button-elt (wrap-primitive string? val)
                   callback
                   (wrap-primitive boolean? enabled)))
(define button button/enabled)

(define (slider/enabled val min max callback [enabled? #t])
  (make-slider-elt (wrap-primitive number? val)
                   (wrap-primitive number? min)
                   (wrap-primitive number? max)
                   callback
                   (wrap-primitive boolean? enabled?)))

(define slider slider/enabled)

(define (drop-down/enabled val choices callback [enabled? #t])
  (make-drop-down-elt (wrap-primitive string? val)
                      (wrap-primitive (flat-contract-predicate (listof string?))
                                      choices)
                      callback
                      (wrap-primitive boolean? enabled?)))
(define drop-down drop-down/enabled)


(define (text-field/enabled val callback [enabled? #t])
  (make-text-field-elt (wrap-primitive string? val)
                       callback
                       (wrap-primitive boolean? enabled?)))
(define text-field text-field/enabled)


(define (canvas/callback a-scene [callback (lambda (world x y) world)])
  (make-canvas-elt (wrap-primitive scene? a-scene) callback))

(define canvas canvas/callback)


(define (box-group/enabled val a-gui [enabled? #t])
  (make-box-group-elt (wrap-primitive string? val)
                      a-gui
                      (wrap-primitive boolean? enabled?)))
(define box-group box-group/enabled)


(define (checkbox/enabled val callback [enabled? #t])
  (make-checkbox-elt (wrap-primitive boolean? val)
                     callback
                     (wrap-primitive boolean? enabled?)))
(define checkbox checkbox/enabled)



;; coerse-primitive-types-to-elts: (listof (or/c elt string scene)) -> (listof elt)
;; Helper to turn strings into string-elts, and images into image-elts.
(define (coerse-primitive-types-to-elts elts)
  (map coerse-primitive-to-elt elts))

;; coerse-primitive-type-to-elt: (or/c elt string scene) -> elt
(define (coerse-primitive-to-elt an-elt)
  (cond [(string? an-elt)
         (message an-elt)]
        [(scene? an-elt)
         (canvas an-elt)]
        [else
         an-elt]))



;; project/inject/gui: elt (world -> sub-world) (world sub-world -> world) -> elt
;; Scoping operator on structure.
(define (project/inject/gui an-elt w->s s->w)
  (match an-elt
    [(struct row-elt (elts))
     (make-row-elt (map (lambda (a-subelt) (project/inject/gui a-subelt w->s s->w))
                        elts))]
    
    [(struct column-elt (elts))
     (make-column-elt (map (lambda (a-subelt) (project/inject/gui a-subelt w->s s->w))
                           elts))]
    
    [(struct box-group-elt (val-f a-subelt enabled?-f))
     (make-box-group-elt (project val-f w->s)
                         (project/inject/gui a-subelt w->s s->w)
                         (project enabled?-f w->s))]

    [(struct string-elt (val-f))
     (make-string-elt (project val-f w->s))]
    
    [(struct canvas-elt (scene-f callback))
     (make-canvas-elt (project scene-f w->s) 
                      (project/inject callback w->s s->w))]
    
    [(struct button-elt (val-f callback enabled?-f))
     (make-button-elt (project val-f w->s)
                      (project/inject callback w->s s->w)
                      (project enabled?-f w->s))]
    
    [(struct drop-down-elt (val-f choices-f callback enabled?-f))
     (make-drop-down-elt (project val-f w->s)
                         (project choices-f w->s)
                         (project/inject callback w->s s->w)
                         (project enabled?-f w->s))]
    
    [(struct text-field-elt (val-f callback enabled?-f))
     (make-text-field-elt (project val-f w->s)
                          (project/inject callback w->s s->w)
                          (project enabled?-f w->s))]
    
    [(struct slider-elt (val-f min-f max-f callback enabled?-f))
     (make-slider-elt (project val-f w->s)
                      (project min-f w->s)
                      (project max-f w->s)
                      (project/inject callback w->s s->w)
                      (project enabled?-f w->s))]
  
    [(struct checkbox-elt (val-f callback enabled?-f))
     (make-checkbox-elt (project val-f w->s)
                        (project/inject callback w->s s->w)
                        (project enabled?-f w->s))]))


;; project: (S -> X) (W -> S) -> (W -> X)
(define (project a-gvalue w->s)
  (lambda (a-world)
    (a-gvalue (w->s a-world))))



;; project/inject: (S X Y -> S) (W -> S) (W S -> W) -> (W X Y -> W)
;; project/inject: (S X -> S) (W -> S) (W S -> W) -> (W X -> W)
;; project/inject: (S -> S) (W -> S) (W S -> W) -> (W -> W)
(define (project/inject a-gcallback w->s s->w)
  (cond
    [(procedure-arity-includes? a-gcallback 3)
     (lambda (a-world v1 v2)
       (s->w a-world (a-gcallback (w->s a-world) v1 v2)))]
    [(procedure-arity-includes? a-gcallback 2)
     (lambda (a-world a-val)
       (s->w a-world (a-gcallback (w->s a-world) a-val)))]
    [(procedure-arity-includes? a-gcallback 1)
     (lambda (a-world)
       (s->w a-world (a-gcallback (w->s a-world))))]))


(provide-primitives col row)

;; NOTE: We're doing this (having pairs of functions, one with and one 
;; without the enabled argument) because provide-higher-order-primitive doesn't
;; support optional arguments.
;;
;; One other deviation that I haven't figured out how to get around yet is
;; that higher-order values are required to be functions in beginner level.  But the design
;; of gui-world asks that we allow primitive values there too for convenience.  Argh.
;;
;; TODO: we need to figure out how to allow non-higher-order values in higher-order
;; position to fit the original design of gui-world.
(provide-higher-order-primitive message (val-f))
(provide-higher-order-primitive button (_ callback))
(provide-higher-order-primitive button/enabled (_ callback enabled?-f))
(provide-higher-order-primitive slider (val-f _ _ callback))
(provide-higher-order-primitive slider/enabled (val-f _ _ callback enabled?-f))
(provide-higher-order-primitive drop-down (val-f choices-f callback))
(provide-higher-order-primitive drop-down/enabled (val-f choices-f callback enabled?-f))
(provide-higher-order-primitive text-field (val-f callback))
(provide-higher-order-primitive text-field/enabled (val-f callback enabled?-f))
(provide-higher-order-primitive checkbox (val-f callback))
(provide-higher-order-primitive checkbox/enabled (val-f callback enabled?-f))
(provide-higher-order-primitive canvas (scene-f))
(provide-higher-order-primitive canvas/callback (scene-f callback))
(provide-higher-order-primitive box-group (_ _))
(provide-higher-order-primitive box-group/enabled (_ _ enabled?-f))

(provide-higher-order-primitive project/inject/gui (_ projection-f injection-f))



(provide/contract [struct elt ()]
                  
                  [struct (row-elt elt) ([elts (listof elt?)])]
                  
                  [struct (column-elt elt) ([elts (listof elt?)])]
                  
                  [struct (box-group-elt elt) ([val-f (gvalueof string?)]
                                               [elt elt?]
                                               [enabled?-f (gvalueof boolean?)])]

                  [struct (string-elt elt) ([val-f (gvalueof string?)])]
                  
                  [struct (canvas-elt elt) ([scene-f (gvalueof scene?)]
                                            [callback (gcallbackof-2 number? number?)])]
                  
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

                  [struct (checkbox-elt elt) ([val-f (gvalueof boolean?)]
                                              [callback (gcallbackof boolean?)]
                                              [enabled?-f (gvalueof boolean?)])]

                  
                  #;[message ((gvalueof* string?) . -> . string-elt?)]
                  
                  #;[button (((gvalueof* string?) 
                            callback/c)
                           ((gvalueof* boolean?)) 
                           . ->* . button-elt?)]
                  
                  #;[slider (((gvalueof* number?) 
                            (gvalueof* number?)
                            (gvalueof* number?)
                            (gcallbackof number?))
                           ((gvalueof* boolean?))
                           . ->* . slider-elt?)]
                  
                  #;[drop-down (((gvalueof* string?)
                               (gvalueof* (listof string?))
                               (gcallbackof string?)) 
                              ((gvalueof* boolean?))
                              . ->* . drop-down-elt?)]
                  
                  #;[text-field (((gvalueof* string?)
                                (gcallbackof string?))
                               ((gvalueof* boolean?))
                               . ->* . text-field-elt?)]
                  
                  #;[checkbox (((gvalueof* boolean?)
                              (gcallbackof boolean?))
                             ((gvalueof* boolean?))
                             . ->* . checkbox-elt?)]
                  
                  #;[canvas (((gvalueof* scene?)) 
                           ((gcallbackof-2 number? number?)) 
                           . ->* .
                           canvas-elt?)]
                  
                  #;[row (() () #:rest (listof (or/c elt? string? scene?)) . ->* . row-elt?)]
                  #;[col (() () #:rest (listof (or/c elt? string? scene?)) . ->* . column-elt?)]
                  #;[box-group (((gvalueof* string?) elt?)
                              ((gvalueof* boolean?))
                              . ->* .
                              box-group-elt?)]
                  
                  #;[project/inject/gui (elt? 
                                 (world/c . -> . subworld/c) 
                                 (world/c subworld/c . -> . world/c)
                                 . -> . 
                                 elt?)]
                  
                  #;[coerse-primitive-to-elt
                     ((or/c elt? string? scene?) . -> . elt?)])