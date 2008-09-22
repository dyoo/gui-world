#lang scheme/base

(require scheme/gui/base
         scheme/match
         scheme/class
         scheme/list
         scheme/bool
         htdp/error
         htdp/image
         mrlib/cache-image-snip
         (only-in lang/htdp-beginner image?)
         (only-in srfi/1 list-index))
(require (for-syntax scheme/base
                     scheme/list
                     scheme/struct-info))

(define ... 'FIXME)




;                                                   
;                                                   
;          ;;;           ;             ;;;          
;     ;;;    ;           ;               ;          
;    ;   ;   ;           ;               ;          
;   ;        ;     ;;;   ;;;;    ;;;     ;     ;;;  
;   ;        ;    ;; ;;  ;; ;;  ;   ;    ;    ;   ; 
;   ;   ;;   ;    ;   ;  ;   ;      ;    ;    ;     
;   ;    ;   ;    ;   ;  ;   ;   ;;;;    ;     ;;;  
;   ;    ;   ;    ;   ;  ;   ;  ;   ;    ;        ; 
;    ;   ;   ;    ;; ;;  ;; ;;  ;  ;;    ;    ;   ; 
;     ;;;     ;;   ;;;   ;;;;    ;; ;     ;;   ;;;  
;                                                   
;                                                   
;                                           ;   ;;  

(define *world* #f)
(define *width* #f)
(define *height* #f)
(define *frame* #f)
(define *form* #f)

(define *on-redraw-callback* #f)
(define *on-tick-callback* #f)
(define *on-tick-frequency* #f)



;                       
;                       
;                       
;     ;;;  ;    ; ;;;;; 
;    ;   ; ;    ;   ;   
;   ;      ;    ;   ;   
;   ;      ;    ;   ;   
;   ;   ;; ;    ;   ;   
;   ;    ; ;    ;   ;   
;   ;    ; ;    ;   ;   
;    ;   ; ;    ;   ;   
;     ;;;   ;;;;  ;;;;; 
;                       
;                       
;                ; ;  ;;



(define-struct form (elt) #:transparent)


(define-struct elt () #:transparent)
;; An element is one of the following:
(define-struct (row-elt elt) (elts) #:transparent)
(define-struct (column-elt elt) (elts) #:transparent)
(define-struct (string-elt elt) (s) #:transparent)
(define-struct (button-elt elt) (label callback enabled?) #:transparent)
(define-struct (drop-down-elt elt) (value choices callback) #:transparent)
(define-struct (text-field-elt elt) (s callback) #:transparent)
(define-struct (slider-elt elt) (v min max callback) #:transparent)
(define-struct (image-elt elt) (img) #:transparent)


;; big-bang: number number world -> void
;; Shows the frame, creates the initial world.
(define (big-bang width height initial-world)
  (set! *width* width)
  (set! *height* height)
  (set! *world* initial-world)
  (set-and-show-frame)
  (change-world! initial-world))


;; set-and-show-frame: -> void
(define (set-and-show-frame)
  (set! *frame* (new frame% 
                     [label ""] 
                     [width *width*]
                     [height *height*]))
  (send *frame* show #t))



;; on-redraw: (world -> form) -> void
;; Initializes the redrawing callback.
(define (on-redraw callback)
  (set! *on-redraw-callback* callback)
  (refresh!))



(define (on-tick freq callback)
  (set! *on-tick-frequency* freq)
  (set! *on-tick-callback* callback)
  (thread (lambda ()
            (let loop ()
              (sleep *on-tick-frequency*)
              (change-world! (*on-tick-callback* *world*))
              (loop))))
  (void))



;; change-world!: world -> void
(define (change-world! new-world)
  (set! *world* new-world)
  (refresh!))


(define (refresh!)
  (when *on-redraw-callback*
    (let ([new-form (*on-redraw-callback* *world*)])
      (unless (equal? *form* new-form)
        (set! *form* new-form)
        (render-form-to-frame new-form *frame*)))))


;; render-form-to-frame: form frame -> void
;; Clears out the contents of the frame, and adds the form elements in.
(define (render-form-to-frame a-form a-frame)
  ;; Returns true if the form element has the exact same structure as the
  ;; gui on screen.
  (define (gui-completely-reusable?)
    (match (send a-frame get-children)
      [(list a-column)
       (send a-column compatible? (form-elt a-form))]
      [else
       #f]))
  
  (define (render-from-scratch!)
    ;; Remove all children, and add the rendered form element as a child.
    (send a-frame change-children (lambda (subareas) '()))
    (render-elt! (form-elt a-form) a-frame)
    (void))
  
  (define (reuse!)2
    (update-elt! (form-elt a-form) 
                 (first (send a-frame get-children))))

  (dynamic-wind
   (lambda ()
     (send a-frame begin-container-sequence))
   (lambda ()
     (cond
       [(gui-completely-reusable?)
        (reuse!)]
       [else
        (render-from-scratch!)]))
   (lambda ()
     (send a-frame end-container-sequence))))






(define world-gui<%> (interface () 
                       compatible?
                       update-with!))


(define world-gui:row%
  (class* horizontal-panel% (world-gui<%>)
    (inherit get-children)
    (define/public (compatible? an-elt)
      (and (row-elt? an-elt)
           (= (length (get-children))
              (length (row-elt-elts an-elt)))
           (andmap (lambda (sub-elt gui-elt)
                     (send gui-elt compatible? sub-elt))
                   (row-elt-elts an-elt)
                   (get-children))))
    
    (define/public (update-with! an-elt)
      (for-each (lambda (sub-elt sub-gui-elt)
                  (send sub-gui-elt update-with! sub-elt))
                (row-elt-elts an-elt)
                (get-children)))
    
    (super-new)))

(define world-gui:column%
  (class* vertical-panel% (world-gui<%>)
    (inherit get-children)

    (define/public (compatible? an-elt)
      (and (column-elt? an-elt)
           (= (length (get-children)) (length (column-elt-elts an-elt)))
           (andmap (lambda (sub-elt gui-elt)
                     (send gui-elt compatible? sub-elt))
                   (column-elt-elts an-elt)
                   (get-children))))

    (define/public (update-with! an-elt)
      (for-each (lambda (sub-elt sub-gui-elt)
                  (send sub-gui-elt update-with! sub-elt))
                (column-elt-elts an-elt)
                (get-children)))

    (super-new)))


(define world-gui:string% 
  (class* message% (world-gui<%>)
    (inherit get-label set-label)
    
    (define/public (compatible? an-elt)
      (string-elt? an-elt))
    
    (define/public (update-with! an-elt)
      (unless (string=? (string-elt-s an-elt) (get-label))
        (set-label (string-elt-s an-elt))))
    
    (super-new [auto-resize #t])))


(define world-gui:button%
  (class* button% (world-gui<%>)
    (inherit get-label set-label is-enabled? enable)
    
    (init-field world-callback)
    
    (define/public (compatible? an-elt)
      (button-elt? an-elt))
    
    (define/public (update-with! an-elt)
      (unless (string=? (button-elt-label an-elt) (get-label))
        (set-label (button-elt-label)))
      (unless (eq? world-callback (button-elt-callback an-elt))
        (set! world-callback (button-elt-callback an-elt)))
      (unless (boolean=? (is-enabled?) (button-elt-enabled? an-elt))
        (enable (button-elt-enabled? an-elt))))
      
    (super-new [callback (lambda (b e)
                           (change-world!
                            (world-callback *world*)))])))


(define world-gui:text-field% 
  (class* text-field% (world-gui<%>)
    (inherit get-value set-value)
    (init-field world-callback)
    
    (define/public (compatible? an-elt)
      (text-field-elt? an-elt))
    
    (define/public (update-with! an-elt)
      (unless (string=? (text-field-elt-s an-elt) (get-value))
        (set-value (text-field-elt-s an-elt)))
      (unless (eq? world-callback (text-field-elt-callback an-elt))
        (set! world-callback (text-field-elt-callback an-elt))))
      
    
    (super-new [callback (lambda (f e)
                           (change-world!
                            (world-callback *world* (get-value))))])))


(define world-gui:drop-down% 
  (class* choice% (world-gui<%>)
    (init-field world-callback)
    (inherit get-string-selection get-number get-string clear append set-selection)
    
    (define/public (compatible? an-elt)
      (drop-down-elt? an-elt))

    (define/public (update-with! an-elt)
      (unless (andmap string=? (get-choices) (drop-down-elt-choices an-elt))
        (clear)
        (for ([choice (drop-down-elt-choices)])
          (append choice)))
      (unless (string=? (get-string-selection) (drop-down-elt-value an-elt))
        (set-selection (list-index (lambda (x) 
                                     (string=? x (drop-down-elt-value an-elt)))
                                   (drop-down-elt-choices an-elt))))
      (unless (eq? world-callback (drop-down-elt-callback an-elt))
        (set! world-callback (drop-down-elt-callback an-elt))))

    ;; get-choices: -> (listof string)
    (define (get-choices)
      (let loop ([i 0])
        (cond [(= i (get-number))
               '()]
              [else
               (cons (get-string i)
                     (loop (add1 i)))])))
    
    (super-new
     [callback (lambda (c e)
                 (change-world! 
                  (world-callback *world* (get-string-selection))))])))



(define world-gui:slider%
  (class* slider% (world-gui<%>)
    (inherit get-value set-value)
    (init min-value
          max-value)
    (init-field world-callback)
    
    (define/public (compatible? an-elt)
      (and (slider-elt? an-elt)
           (= -min-value (slider-elt-min an-elt))
           (= -max-value (slider-elt-max an-elt))))
    
    (define/public (update-with! an-elt)
      (unless (= (slider-elt-v an-elt) (get-value))
        (set-value (slider-elt-v an-elt)))
      (unless (eq? world-callback
                   (slider-elt-callback an-elt))
        (set! world-callback (slider-elt-callback an-elt))))
   
    (define -min-value min-value)
    (define -max-value max-value)
    (super-new
     [min-value min-value]
     [max-value max-value]
     [callback (lambda (s e)
                 (change-world! 
                  (world-callback *world* (get-value))))])))



(define world-gui:image%
  (class* editor-canvas% (world-gui<%>)
    (inherit get-editor min-width min-height)
    
    (define/public (compatible? an-elt)
      (image-elt? an-elt))
    
    (define/public (update-with! an-elt)
      (let ([editor (get-editor)]
            [img (send (image-elt-img an-elt) copy)])
        (min-width (image-width img))
        (min-height (image-height img))
        (dynamic-wind 
         (lambda () 
           (send editor begin-edit-sequence))
         (lambda () 
           (send editor erase)
           (send editor insert img 0 0))
         (lambda () 
           (send editor end-edit-sequence)))))
    
    (super-new)))



;; render-elt!: elt container% -> world-gui<%>
;; Adds an elt to the gui container. 
(define (render-elt! an-elt a-container)
  (match an-elt
    [(struct row-elt (elts))
     (let ([row-container 
            (new world-gui:row% [parent a-container])])
       (for ([sub-elt elts])
         (render-elt! sub-elt row-container))
       row-container)]
    
    [(struct column-elt (elts))
     (let ([column-container
            (new world-gui:column% [parent a-container])])
       (for ([sub-elt elts])
         (render-elt! sub-elt column-container))
       column-container)]
    
    [(struct string-elt (s))
     (new world-gui:string% [label s]
          [parent a-container])]
    
    [(struct button-elt (label callback enabled?))
     (new world-gui:button% [label label]
          [parent a-container]
          [world-callback callback]
          [enabled enabled?])]
    
    [(struct text-field-elt (v callback))
     (new world-gui:text-field% 
          [label #f]
          [parent a-container]
          [init-value v]
          [world-callback callback])]
    
    [(struct drop-down-elt (val choices callback))
     (new world-gui:drop-down% 
          [label #f]
          [choices choices]
          [selection (list-index (lambda (x) 
                                   (string=? x val))
                                 choices)]
          [parent a-container]
          [world-callback callback])]
    
    [(struct slider-elt (val min max callback))
     (new world-gui:slider% 
          [label #f]
          [parent a-container]
          [min-value min]
          [max-value max]
          [init-value val]
          [world-callback callback])]
    
    [(struct image-elt (an-image-snip))
     (let* ([pasteboard (new pasteboard%)]
            [img-snip (send an-image-snip copy)]
            [canvas (new world-gui:image%
                         [parent a-container]
                         [min-width (image-width img-snip)]
                         [min-height (image-height img-snip)]
                         [stretchable-width #f]
                         [stretchable-height #f]
                         [editor pasteboard])])
       (send pasteboard insert img-snip 0 0)
       canvas)]))



;; update-elt!: elt world-gui<%> -> void
;; Adds an elt to the gui container. 
(define (update-elt! an-elt gui-elt)
  (send gui-elt update-with! an-elt))




;; make-form: element+ -> form
(define (-make-form first-elt . rest-elements)
  (make-form 
   (make-column-elt
    (coerse-primitive-types-to-elts (cons first-elt rest-elements)))))


;; coerse-primitive-types-to-elts: (listof (or/c elt string)) -> (listof elt)
;; Helper to turn strings into string-elts, and images into image-elts.
(define (coerse-primitive-types-to-elts elts)
  (map (lambda (elt)
         (cond [(string? elt)
                (make-string-elt (escape-label elt))]
               [(is-a? elt cache-image-snip%)
                (make-image-elt elt)]
               [else
                elt]))
       elts))



;; make-button: string (world -> world) [enabled? boolean] -> element
(define (make-button label callback [enabled? #t])
  (make-button-elt (escape-label label) callback enabled?))

;; make-row: element+ -> element
(define (make-row first-elt . rest-elts)
  (make-row-elt (coerse-primitive-types-to-elts (cons first-elt rest-elts))))

;; make-column: element+ -> element
(define (make-column first-elt . rest-elts)
  (make-column-elt (coerse-primitive-types-to-elts (cons first-elt rest-elts))))

;; make-drop-down: string (listof string) (world string -> world) -> element
(define (make-drop-down default-value choices callback)
  (unless (member default-value choices)
    (error 'make-drop-down "Value ~s not in the choices ~s" default-value choices))
  (make-drop-down-elt (escape-label default-value) (map escape-label choices) callback))

;; make-text-field: string (world string -> world) -> element
(define (make-text-field default-value callback)
  (make-text-field-elt (escape-label default-value) callback))

;; make-slider: number number number (world number -> world) -> element
(define (make-slider v min max callback)
  (make-slider-elt v min max callback))


;; escape-label: string -> string
;; Escapes the special character used for underlining, since we don't want the
;; underlining behavior.
(define (escape-label a-label-str)
  (regexp-replace* #rx"&" a-label-str "&&"))




;                              
;                              
;            ;                 
;   ;    ;                     
;   ;;  ;;                     
;   ;;  ;; ;;;     ;;;    ;;;  
;   ; ;; ;   ;    ;   ;  ;;  ; 
;   ; ;; ;   ;    ;      ;     
;   ; ;; ;   ;     ;;;   ;     
;   ;    ;   ;        ;  ;     
;   ;    ;   ;    ;   ;  ;;  ; 
;   ;    ; ;;;;;   ;;;    ;;;  
;                              
;                              
;                        ; ;;  



;; random-choice: (listof X) -> X
;; Given a list of elements, chooses one of them randomly.
(define (random-choice elts)
  (list-ref elts
            (random (length elts))))



;; Not really a part of GUI.
;; Convenient syntax for defining all the replacing-attribute functions,
;; given a structure id.
;; Usage:
;; If we have a
;;     (define-struct posn (x y z))
;; then
;;     (define-replacers posn)
;; will expand out to definitions for replace-posn-x, replace-posn-y, replace-posn-z.
;; Each replacer takes the struct val and an attribute value, and produces a new struct val.
(define-syntax (define-updaters stx)
  (syntax-case stx ()
    [(_ a-struct-type)
     (let* ([info (extract-struct-info (syntax-local-value #'a-struct-type))]
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
                  (fourth info))])
       (with-syntax ([(accessor ...) fields]
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
                    (define (update a-struct-val new-val)
                      (struct-copy a-struct-type a-struct-val
                                   (accessor new-val)))
                    ...))])
           result)))]))







;; Copy-and-paste from htdp/world

(define (place-image image x y scene)
  (check-image 'place-image image "first")
  (check-arg 'place-image (number? x) 'integer "second" x)
  (check-arg 'place-image (number? y) 'integer "third" y)
  (check-scene 'place-image scene "fourth")
  (let ([x (number->integer x)]
        [y (number->integer y)])
    (place-image0 image x y scene)))

;; Symbol Any String String *-> Void
(define (check-image tag i rank . other-message)
  (if (and (pair? other-message) (string? (car other-message)))
      (check-arg tag (image? i) (car other-message) rank i)
      (check-arg tag (image? i) "image" rank i)))

;; Symbol Any String -> Void
(define (check-scene tag i rank)
  (if (image? i)
      (unless (scene? i)
        (error tag "scene expected, given image whose pinhole is at (~s,~s) instead of (0,0)"
               (pinhole-x i) (pinhole-y i)))
      (check-arg tag #f "image" rank i)))

(define (place-image0 image x y scene)
  (define sw (image-width scene))
  (define sh (image-height scene))
  (define ns (overlay/xy scene x y image))
  (define nw (image-width ns))
  (define nh (image-height ns))
  (if (and (= sw nw) (= sh nh)) ns (shrink ns 0 0 (- sw 1) (- sh 1)))) 

;; Symbol Any String -> Void
(define (check-pos tag c rank)
  (check-arg tag (and (number? c) (> (number->integer c) 0))
             "positive integer" rank c))

(define (scene? i) (and (= 0 (pinhole-x i)) (= 0 (pinhole-y i))))

;; Number -> Integer
(define (number->integer x)
  (inexact->exact (floor x)))

(define (empty-scene width height)
  (check-pos 'empty-scene width "first")
  (check-pos 'empty-scene height "second")    
  (put-pinhole 
   (overlay (rectangle width height 'solid 'white)
            (rectangle width height 'outline 'black))
   0 0))




(provide 
 
 big-bang
 on-redraw
 on-tick
 
 (rename-out [-make-form make-form])
 make-button
 make-row
 make-column
 make-drop-down
 make-text-field
 make-slider
 
 
 place-image
 empty-scene
 
 random-choice
 define-updaters)