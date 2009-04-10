#lang scheme/base

(require scheme/contract
         scheme/list
         htdp/image
         htdp/error
         scheme/match
         "prim.ss"
         (only-in lang/htdp-beginner image?))


(define-struct css (mappings) #:prefab)

(define (-make-css)
  (make-css (make-immutable-hash '())))

;; css-update: css elt symbol X -> css
(define (css-update a-css an-elt a-name a-value)
  (match a-css 
    [(struct css (h))
     (make-css (hash-set h (list an-elt a-name) a-value))]))

;; css-lookup: css elt symbol [(-> X)] -> X
(define (css-lookup a-css an-elt a-name 
                    (default (lambda () 
                               (error 'css-lookup "Couldn't find ~s" a-name))))
  (match a-css
    [(struct css (h))
     (hash-ref h (list an-elt a-name) default)]))




(define-struct elt () #:prefab)
;; An gui element is one of the following:

(define-struct (row-elt elt) (elts css-f) #:prefab)       ;; fixme: elts should be dynamic
(define-struct (column-elt elt) (elts css-f) #:prefab)    ;; fixme: elts should be dynamic

(define-struct (box-group-elt elt) (val-f elt enabled?-f css-f) #:prefab)
(define-struct (pasteboard-elt elt) (elts-f css-f) #:prefab)

(define-struct (displayable-elt elt) (val-f css-f) #:prefab)
(define-struct (canvas-elt elt) (scene-f callback css-f) #:prefab)
(define-struct (button-elt elt) (val-f callback enabled?-f css-f) #:prefab)
(define-struct (drop-down-elt elt) (val-f choices-f callback enabled?-f css-f) #:prefab)
(define-struct (text-field-elt elt) (val-f callback enabled?-f css-f) #:prefab)
(define-struct (slider-elt elt) (val-f min-f max-f callback enabled?-f css-f) #:prefab)
(define-struct (checkbox-elt elt) (label-f val-f callback enabled?-f css-f) #:prefab)



;; elt-css-f: elt -> (world css -> css)
(define (elt-css-f an-elt)
  (match an-elt
    [(struct row-elt (elts css-f))
     css-f]
    [(struct column-elt (elts css-f)) 
     css-f]
    [(struct box-group-elt (val-f elt enabled?-f css-f))
     css-f]
    [(struct pasteboard-elt  (elts-f css-f))
     css-f]
    [(struct displayable-elt (val-f css-f))
     css-f]
    [(struct canvas-elt (scene-f callback css-f))
     css-f]
    [(struct button-elt  (val-f callback enabled?-f css-f))
     css-f]
    [(struct drop-down-elt  (val-f choices-f callback enabled?-f css-f))
     css-f]
    [(struct text-field-elt (val-f callback enabled?-f css-f))
     css-f]
    [(struct slider-elt (val-f min-f max-f callback enabled?-f css-f))
     css-f]
    [(struct checkbox-elt (label-f val-f callback enabled?-f css-f))
     css-f]))





(define world/c any/c)
(define subworld/c any/c)


(define (gvalueof t)
  (world/c elt? . -> . t))


(define gcallbackof 
  (case-lambda
    [()   
     (world/c elt? . -> . world/c)]
    [(t)
     (world/c elt? t . -> . world/c)]
    [(t1 t2)
     (world/c elt? t1 t2 . -> . world/c)]))



;; displayable?: any -> boolean
(define (displayable? datum)
  (or (string? datum)
      (symbol? datum)
      (number? datum)
      (boolean? datum)))


;; displayable->string: displayable -> string
(define (displayable->string datum)
  (match datum 
    [(? string?)
     datum]
    [(? symbol?)
     (symbol->string datum)]
    [(? number?) 
     (number->string datum)]
    [(? boolean?)
     (cond [datum "true"]
           [else "false"])]))



;; scene?: any -> boolean
(define (scene? i)
  (and (image? i)
       (= 0 (pinhole-x i))
       (= 0 (pinhole-y i))))


;; wrap-primitive: symbol (any/c -> boolean) (or/c X (world -> X) (world elt -> X)) -> (world elt -> X)
(define (wrap-primitive a-name a-pred a-gvalue)
  (cond
    [(a-pred a-gvalue)
     (lambda (a-world an-elt) a-gvalue)]
    [(procedure? a-gvalue)
     (cond [(procedure-arity-includes? a-gvalue 2)
            ;; Fixme: add wrapper to check return type.
            a-gvalue]

           [(procedure-arity-includes? a-gvalue 1)
            ;; Fixme: add wrapper to check return type.
            (lambda (world elt)
              (a-gvalue world))]

           [else
            (error a-name
                      "Expected procedure of arity 1 or 2, but got ~s"
                      a-gvalue)])]
    [else 
     (error a-name
               "Expected either a <~s> or a (world -> ~s) function, but got ~s instead"
               (object-name a-pred)
               (object-name a-pred)
               a-gvalue)]))




;; wrap-css: symbol any/c -> (world elt css -> css)
(define (wrap-css for-name a-css-function)
  (cond
    [(procedure? a-css-function) 
     (cond  
       [(procedure-arity-includes? a-css-function 3)
        a-css-function]
       
       [(procedure-arity-includes? a-css-function 2)
        (lambda (world me css)
          (a-css-function world css))]
       [else
        (error 'wrap-css
               "Procedure arity of ~s not in [2, 3]"
               a-css-function)])]
    [else
     (lambda (world me css)
       css)]))


;; wrap-gcallback: symbol gcallback-or-value number -> gcallback
(define (wrap-gcallback for-name a-callback-or-value min-arity)
  (cond
    [(procedure? a-callback-or-value) 
     (cond  
       [(procedure-arity-includes? a-callback-or-value (add1 min-arity))
        a-callback-or-value]
       
       [(procedure-arity-includes? a-callback-or-value min-arity)
        (lambda args
          (let ([world (first args)]
                [me (second args)]
                [other-args (rest (rest args))])
          (apply a-callback-or-value world other-args)))]
       [else
        (error 'for-name
               (format "Procedure arity of ~s not in [~a, ~a]"
                       a-callback-or-value
                       min-arity (add1 min-arity)))])]
    [else
     (lambda args
       a-callback-or-value)]))




;; default-css-f: world elt css -> css
(define (default-css-f world elt css)
  css)


(define (row #:css (css-f default-css-f)
             . elts)
  (make-row-elt (coerse-primitive-types-to-elts elts)
                (wrap-css 'row css-f)))


(define (col #:css (css-f default-css-f)
             . elts)
  (make-column-elt (coerse-primitive-types-to-elts elts)
                   (wrap-css 'col css-f)))


(define (pasteboard elts-f #:css-f (css-f default-css-f))
  (make-pasteboard-elt (wrap-primitive 'pasteboard 
                                       (flat-contract-predicate (listof elt?))
                                       elts-f)
                       (wrap-css 'pasteboard css-f)
                       ))


(define (message a-gvalue
                 #:css (css-f default-css-f))
  (make-displayable-elt (wrap-primitive 'message displayable? a-gvalue)
                        (wrap-css 'message css-f)))


(define (button val callback 
                #:enabled [enabled #t]
                #:css-f (css-f default-css-f))
  (make-button-elt (wrap-primitive 'button displayable? val)
                   (wrap-gcallback 'button callback 1)
                   (wrap-primitive 'button boolean? enabled)
                   (wrap-css 'button/enabled css-f)))


(define (slider val min max callback
                #:enabled [enabled? #t]
                #:css (css-f default-css-f))
  (make-slider-elt (wrap-primitive 'slider number? val)
                   (wrap-primitive 'slider number? min)
                   (wrap-primitive 'slider number? max)
                   (wrap-gcallback 'slider callback 2)
                   (wrap-primitive 'slider boolean? enabled?)
                   (wrap-css 'slider/enabled css-f)))


(define (drop-down val choices callback
                   #:enabled [enabled? #t]
                   #:css (css-f default-css-f))
  (make-drop-down-elt (wrap-primitive 'drop-down displayable? val)
                      (wrap-primitive 'drop-down (flat-contract-predicate (listof displayable?))
                                      choices)
                      (wrap-gcallback 'drop-down callback 2)
                      (wrap-primitive 'drop-down boolean? enabled?)
                      (wrap-css 'drop-down/enabled css-f)))


(define (text-field val callback
                    #:enabled [enabled? #t]
                    #:css (css-f default-css-f))
  (make-text-field-elt (wrap-primitive 'text-field displayable? val)
                       (wrap-primitive 'text-field callback 2)
                       (wrap-primitive 'text-field boolean? enabled?)
                       (wrap-css 'text-field/enabled css-f)))


(define (canvas a-scene 
                #:callback (callback (lambda (world me x y) world))
                #:css (css-f default-css-f))
  (make-canvas-elt (wrap-primitive 'canvas scene? a-scene) 
                   (wrap-gcallback 'canvas callback 3)
                   (wrap-css 'canvas css-f)))



(define (box-group val a-gui
                   #:enabled [enabled? #t]
                   #:css (css-f default-css-f))
  (make-box-group-elt (wrap-primitive 'box-group displayable? val)
                      (coerse-primitive-to-elt a-gui)
                      (wrap-primitive 'box-group boolean? enabled?)
                      (wrap-css 'box-group/enabled css-f)))



(define (checkbox label val callback 
                  #:enabled [enabled? #t]
                  #:css (css-f default-css-f))
  (make-checkbox-elt (wrap-primitive 'checkbox displayable? label)
                     (wrap-primitive 'checkbox boolean? val)
                     (wrap-gcallback 'checkbox callback 2)
                     (wrap-primitive 'checkbox boolean? enabled?)
                     (wrap-css 'checkbox/enabled css-f)))



;; coerse-primitive-types-to-elts: (listof (or/c elt displayable scene)) -> (listof elt)
;; Helper to turn displayables into displayable-elts, and images into image-elts.
(define (coerse-primitive-types-to-elts elts)
  (map coerse-primitive-to-elt elts))


;; coerse-primitive-type-to-elt: (or/c elt displayable scene) -> elt
(define (coerse-primitive-to-elt an-elt)
  (cond [(displayable? an-elt)
         (message an-elt)]
        [(scene? an-elt)
         (canvas an-elt)]
        [else
         an-elt]))



;; project/inject/gui: elt (world -> sub-world) (world sub-world -> world) -> elt
;; Scoping operator on structure.
(define (project/inject/gui an-elt w->s s->w)
  (match an-elt
    [(struct row-elt (elts css-f))
     (make-row-elt (map (lambda (a-subelt) (project/inject/gui a-subelt w->s s->w))
                        elts))]
    
    [(struct column-elt (elts css-f))
     (make-column-elt (map (lambda (a-subelt) (project/inject/gui a-subelt w->s s->w))
                           elts))]
    
    [(struct box-group-elt (val-f a-subelt enabled?-f css-f))
     (make-box-group-elt (project val-f w->s)
                         (project/inject/gui a-subelt w->s s->w)
                         (project enabled?-f w->s))]
    
    [(struct pasteboard-elt (elts css-f))
     (make-pasteboard-elt (map (lambda (a-subelt) (project/inject/gui a-subelt w->s s->w))
                               elts)
                          (project/inject-1 css-f w->s s->w))]
    
    [(struct displayable-elt (val-f css-f))
     (make-displayable-elt (project val-f w->s))]
    
    [(struct canvas-elt (scene-f callback css-f))
     (make-canvas-elt (project scene-f w->s) 
                      (project/inject-2 callback w->s s->w))]
    
    [(struct button-elt (val-f callback enabled?-f css-f))
     (make-button-elt (project val-f w->s)
                      (project/inject-0 callback w->s s->w)
                      (project enabled?-f w->s))]
    
    [(struct drop-down-elt (val-f choices-f callback enabled?-f css-f))
     (make-drop-down-elt (project val-f w->s)
                         (project choices-f w->s)
                         (project/inject-1 callback w->s s->w)
                         (project enabled?-f w->s))]
    
    [(struct text-field-elt (val-f callback enabled?-f css-f))
     (make-text-field-elt (project val-f w->s)
                          (project/inject-1 callback w->s s->w)
                          (project enabled?-f w->s))]
    
    [(struct slider-elt (val-f min-f max-f callback enabled?-f css-f))
     (make-slider-elt (project val-f w->s)
                      (project min-f w->s)
                      (project max-f w->s)
                      (project/inject-1 callback w->s s->w)
                      (project enabled?-f w->s))]
    
    [(struct checkbox-elt (label-f val-f callback enabled?-f css-f))
     (make-checkbox-elt (project label-f w->s)
                        (project val-f w->s)
                        (project/inject-1 callback w->s s->w)
                        (project enabled?-f w->s))]))


;; project: (S -> X) (W -> S) -> (W -> X)
(define (project a-gvalue w->s)
  (lambda (a-world)
    (a-gvalue (w->s a-world))))



;; project/inject-2: (S X Y -> S) (W -> S) (W S -> W) -> (W X Y -> W)
;; project/inject-1: (S X -> S) (W -> S) (W S -> W) -> (W X -> W)
;; project/inject-0: (S -> S) (W -> S) (W S -> W) -> (W -> W)
(define (project/inject-2 a-gcallback w->s s->w)
  (lambda (a-world v1 v2)
    (s->w a-world (a-gcallback (w->s a-world) v1 v2))))

(define (project/inject-1 a-gcallback w->s s->w)
  (lambda (a-world a-val)
    (s->w a-world (a-gcallback (w->s a-world) a-val))))

(define (project/inject-0 a-gcallback w->s s->w)
  (lambda (a-world)
    (s->w a-world (a-gcallback (w->s a-world)))))



(provide col 
         row
         pasteboard
         message 
         button
         slider 
         drop-down 
         text-field
         checkbox
         canvas
         box-group
         project/inject/gui)


(define css-f/c
  (any/c elt? css? . -> . css?))


(provide/contract [struct elt ()]
                  
                  [struct (row-elt elt) ([elts (listof elt?)]
                                         [css-f css-f/c])]
                  
                  [struct (column-elt elt) ([elts (listof elt?)]
                                            [css-f css-f/c])]
                  
                  [struct (box-group-elt elt) ([val-f (gvalueof displayable?)]
                                               [elt elt?]
                                               [enabled?-f (gvalueof boolean?)]
                                               [css-f css-f/c])]
                  
                  [struct (pasteboard-elt elt) ([elts-f (gvalueof (listof elt?))]
                                                ;; 
                                                [css-f css-f/c]
                                                )]
                  
                  [struct (displayable-elt elt) ([val-f (gvalueof displayable?)]
                                                 [css-f css-f/c])]
                  
                  [struct (canvas-elt elt) ([scene-f (gvalueof scene?)]
                                            [callback (gcallbackof number? number?)]
                                            [css-f css-f/c])]
                  
                  [struct (button-elt elt) ([val-f (gvalueof displayable?)]
                                            [callback (gcallbackof)]
                                            [enabled?-f (gvalueof boolean?)]
                                            [css-f css-f/c])]
                  
                  [struct (drop-down-elt elt) ([val-f (gvalueof displayable?)]
                                               [choices-f (gvalueof (listof displayable?))]
                                               [callback (gcallbackof displayable?)]
                                               [enabled?-f (gvalueof boolean?)]
                                               [css-f css-f/c])]
                  
                  [struct (text-field-elt elt) ([val-f (gvalueof displayable?)]
                                                [callback (gcallbackof displayable?)]
                                                [enabled?-f (gvalueof boolean?)]
                                                [css-f css-f/c])]
                  
                  [struct (slider-elt elt) ([val-f (gvalueof number?)]
                                            [min-f (gvalueof number?)]
                                            [max-f (gvalueof number?)]
                                            [callback (gcallbackof number?)]
                                            [enabled?-f (gvalueof boolean?)]
                                            [css-f css-f/c])]
                  
                  [struct (checkbox-elt elt) ([label-f (gvalueof displayable?)]
                                              [val-f (gvalueof boolean?)]
                                              [callback (gcallbackof boolean?)]
                                              [enabled?-f (gvalueof boolean?)]
                                              [css-f css-f/c])]
                  
                  [displayable? (any/c . -> . boolean?)]
                  [displayable->string (displayable? . -> . displayable?)]
                  
                  
                  [rename -make-css make-css (-> css?)]
                  [css-lookup
                   ((css? elt? symbol?) ((-> any/c)) . ->* . any)]
                  [css-update
                   (css? elt? symbol? any/c . -> . css?)]
                  [elt-css-f
                   (elt? . -> . css-f/c)]
                  )