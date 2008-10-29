#lang scheme/base

(require scheme/contract
         htdp/image
         htdp/error
         scheme/match
         "prim.ss"
         (only-in lang/htdp-beginner image?))


(define-struct elt () #:transparent)
;; An gui element is one of the following:

(define-struct (row-elt elt) (elts) #:transparent)
(define-struct (column-elt elt) (elts) #:transparent)
(define-struct (box-group-elt elt) (val-f elt enabled?-f) #:transparent)
(define-struct (displayable-elt elt) (val-f) #:transparent)
(define-struct (canvas-elt elt) (scene-f callback) #:transparent)
(define-struct (button-elt elt) (val-f callback enabled?-f) #:transparent)
(define-struct (drop-down-elt elt) (val-f choices-f callback enabled?-f) #:transparent)
(define-struct (text-field-elt elt) (val-f callback enabled?-f) #:transparent)
(define-struct (slider-elt elt) (val-f min-f max-f callback enabled?-f) #:transparent)
(define-struct (checkbox-elt elt) (label-f val-f callback enabled?-f) #:transparent)



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
      (number? datum)
      (boolean? datum)))


;; displayable->string: displayable -> string
(define (displayable->string datum)
  (match datum 
    [(? string?)
     datum]
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


;; wrap-primitive: (any/c -> boolean) (or/c X (world -> X)) -> (world -> X)
(define (wrap-primitive a-name a-pred a-gvalue)
  (cond
    [(a-pred a-gvalue)
     (lambda (a-world) a-gvalue)]
    [(procedure? a-gvalue)
     (check-fun-res a-gvalue a-pred (object-name a-pred))]
    [else 
     (gw-error a-name
               "Expected either a <~s> or a (world -> ~s) function, but got ~s instead"
               (object-name a-pred)
               (object-name a-pred)
               a-gvalue)]))


(define-struct (gui-world-exn exn) ())

(define (gw-error name fmt . args)
  (raise (make-gui-world-exn (string-append (format "~a: " name) (apply format fmt args))
                             (current-continuation-marks))))

(define (row . elts)
  (make-row-elt (coerse-primitive-types-to-elts elts)))

(define (col . elts)
  (make-column-elt (coerse-primitive-types-to-elts elts)))


(define (message a-gvalue)
  (make-displayable-elt (wrap-primitive 'message displayable? a-gvalue)))


(define (button/enabled val callback [enabled #t])
  (make-button-elt (wrap-primitive 'button/enabled displayable? val)
                   callback
                   (wrap-primitive 'button/enabled boolean? enabled)))

(define (button val callback)
  (make-button-elt (wrap-primitive 'button displayable? val)
                   callback
                   (wrap-primitive 'button boolean? #t)))


(define (slider/enabled val min max callback [enabled? #t])
  (make-slider-elt (wrap-primitive 'slider/enabled number? val)
                   (wrap-primitive 'slider/enabled number? min)
                   (wrap-primitive 'slider/enabled number? max)
                   callback
                   (wrap-primitive 'slider/enabled boolean? enabled?)))

(define (slider val min max callback)
  (make-slider-elt (wrap-primitive 'slider number? val)
                   (wrap-primitive 'slider number? min)
                   (wrap-primitive 'slider number? max)
                   callback
                   (wrap-primitive 'slider boolean? #t)))
  

(define (drop-down/enabled val choices callback [enabled? #t])
  (make-drop-down-elt (wrap-primitive 'drop-down/enabled displayable? val)
                      (wrap-primitive 'drop-down/enabled (flat-contract-predicate (listof displayable?))
                                      choices)
                      callback
                      (wrap-primitive 'drop-down/enabled boolean? enabled?)))

(define (drop-down val choices callback)
  (make-drop-down-elt (wrap-primitive 'drop-down displayable? val)
                      (wrap-primitive 'drop-down (flat-contract-predicate (listof displayable?))
                                      choices)
                      callback
                      (wrap-primitive 'drop-down boolean? #t)))



(define (text-field/enabled val callback [enabled? #t])
  (make-text-field-elt (wrap-primitive 'text-field/enabled displayable? val)
                       callback
                       (wrap-primitive 'text-field/enabled boolean? enabled?)))

(define (text-field val callback)
  (make-text-field-elt (wrap-primitive 'text-field displayable? val)
                       callback
                       (wrap-primitive 'text-field boolean? #t)))


(define (canvas/callback a-scene [callback (lambda (world x y) world)])
  (make-canvas-elt (wrap-primitive 'canvas/callback scene? a-scene) callback))

(define (canvas a-scene)
  (make-canvas-elt (wrap-primitive 'canvas scene? a-scene) (lambda (world x y) world)))



(define (box-group/enabled val a-gui [enabled? #t])
  (make-box-group-elt (wrap-primitive 'box-group/enabled displayable? val)
                      a-gui
                      (wrap-primitive 'box-group/enabled boolean? enabled?)))

(define (box-group val a-gui)
  (make-box-group-elt (wrap-primitive 'box-group displayable? val)
                      a-gui
                      (wrap-primitive 'box-group boolean? #t)))


(define (checkbox/enabled label val callback [enabled? #t])
  (make-checkbox-elt (wrap-primitive 'checkbox/enabled displayable? label)
                     (wrap-primitive 'checkbox/enabled boolean? val)
                     callback
                     (wrap-primitive 'checkbox/enabled boolean? enabled?)))

(define (checkbox label val callback)
  (make-checkbox-elt (wrap-primitive 'checkbox displayable? label)
                     (wrap-primitive 'checkbox boolean? val)
                     callback
                     (wrap-primitive 'checkbox boolean? #t)))




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

    [(struct displayable-elt (val-f))
     (make-displayable-elt (project val-f w->s))]
    
    [(struct canvas-elt (scene-f callback))
     (make-canvas-elt (project scene-f w->s) 
                      (project/inject-2 callback w->s s->w))]
    
    [(struct button-elt (val-f callback enabled?-f))
     (make-button-elt (project val-f w->s)
                      (project/inject-0 callback w->s s->w)
                      (project enabled?-f w->s))]
    
    [(struct drop-down-elt (val-f choices-f callback enabled?-f))
     (make-drop-down-elt (project val-f w->s)
                         (project choices-f w->s)
                         (project/inject-1 callback w->s s->w)
                         (project enabled?-f w->s))]
    
    [(struct text-field-elt (val-f callback enabled?-f))
     (make-text-field-elt (project val-f w->s)
                          (project/inject-1 callback w->s s->w)
                          (project enabled?-f w->s))]
    
    [(struct slider-elt (val-f min-f max-f callback enabled?-f))
     (make-slider-elt (project val-f w->s)
                      (project min-f w->s)
                      (project max-f w->s)
                      (project/inject-1 callback w->s s->w)
                      (project enabled?-f w->s))]
  
    [(struct checkbox-elt (label-f val-f callback enabled?-f))
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
         message 
         button
         button/enabled
         slider 
         slider/enabled 
         drop-down 
         drop-down/enabled
         text-field
         text-field/enabled
         checkbox
         checkbox/enabled
         canvas
         canvas/callback
         box-group
         box-group/enabled
         project/inject/gui)


(provide/contract [struct elt ()]
                  
                  [struct (row-elt elt) ([elts (listof elt?)])]
                  
                  [struct (column-elt elt) ([elts (listof elt?)])]
                  
                  [struct (box-group-elt elt) ([val-f (gvalueof displayable?)]
                                               [elt elt?]
                                               [enabled?-f (gvalueof boolean?)])]
                  
                  [struct (displayable-elt elt) ([val-f (gvalueof displayable?)])]
                  
                  [struct (canvas-elt elt) ([scene-f (gvalueof scene?)]
                                            [callback (gcallbackof-2 number? number?)])]
                  
                  [struct (button-elt elt) ([val-f (gvalueof displayable?)]
                                            [callback callback/c]
                                            [enabled?-f (gvalueof boolean?)])]
                  
                  [struct (drop-down-elt elt) ([val-f (gvalueof displayable?)]
                                               [choices-f (gvalueof (listof displayable?))]
                                               [callback (gcallbackof displayable?)]
                                               [enabled?-f (gvalueof boolean?)])]
                  
                  [struct (text-field-elt elt) ([val-f (gvalueof displayable?)]
                                                [callback (gcallbackof displayable?)]
                                                [enabled?-f (gvalueof boolean?)])]
                  
                  [struct (slider-elt elt) ([val-f (gvalueof number?)]
                                            [min-f (gvalueof number?)]
                                            [max-f (gvalueof number?)]
                                            [callback (gcallbackof number?)]
                                            [enabled?-f (gvalueof boolean?)])]
                  
                  [struct (checkbox-elt elt) ([label-f (gvalueof displayable?)]
                                              [val-f (gvalueof boolean?)]
                                              [callback (gcallbackof boolean?)]
                                              [enabled?-f (gvalueof boolean?)])]
                  
                  [displayable? (any/c . -> . boolean?)]
                  [displayable->string (displayable? . -> . displayable?)])