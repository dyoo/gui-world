#lang scheme/base

(require scheme/gui/base
         scheme/match
         scheme/class
         scheme/list
         scheme/bool
         mrlib/cache-image-snip
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
  
  (define (reuse!)
    (update-elt! (form-elt a-form) 
                 (first (send a-frame get-children))))

  (cond
    [(gui-completely-reusable?)
     (reuse!)]
    [else
     (render-from-scratch!)]))






(define world-gui<%> (interface () 
                       compatible?
                       update-with!))


(define world-gui:row%
  (class* horizontal-panel% (world-gui<%>)
    (inherit get-children)
    (define/public (compatible? an-elt)
      (and (row-elt? an-elt)
           (andmap (lambda (sub-elt gui-elt)
                     (send gui-elt compatible? sub-elt))
                   (row-elt-elts an-elt)
                   (get-children))))
    
    (define/public (update-with! an-elt)
      (for-each (lambda (sub-elt sub-gui-elt)
                  (send sub-gui-elt update-with sub-elt))
                (row-elt-elts an-elt)
                (get-children)))
    
    (super-new)))

(define world-gui:column%
  (class* vertical-panel% (world-gui<%>)
    (inherit get-children)

    (define/public (compatible? an-elt)
      (and (column-elt? an-elt)
           (andmap (lambda (sub-elt gui-elt)
                     (send gui-elt compatible? sub-elt))
                   (column-elt-elts an-elt)
                   (get-children))))

    (define/public (update-with! an-elt)
      (for-each (lambda (sub-elt sub-gui-elt)
                  (send sub-gui-elt update-with sub-elt))
                (column-elt-elts an-elt)
                (get-children)))

    (super-new)))


(define world-gui:string% 
  (class* message% (world-gui<%>)
    (inherit get-text set-label)
    
    (define/public (compatible? an-elt)
      (string-elt? an-elt))
    
    (define/public (update-with! an-elt)
      (unless (string=? (string-elt-s an-elt) (get-text))
        (set-label (string-elt-s an-elt))))
    
    (super-new)))


(define world-gui:button%
  (class* button% (world-gui<%>)
    (inherit get-label set-label is-enabled? enable)
    
    (init-field inner-callback)
    
    (define/public (compatible? an-elt)
      (button-elt? an-elt))
    
    (define/public (update-with! an-elt)
      (unless (string=? (button-elt-label an-elt) (get-label))
        (set-label (button-elt-label)))
      (unless (eq? inner-callback (button-elt-callback an-elt))
        (set! inner-callback (button-elt-callback an-elt)))
      (unless (boolean=? (is-enabled?) (button-elt-enabled? an-elt))
        (enable (button-elt-enabled? an-elt))))
      
    (super-new [callback (lambda (b e)
                           (change-world!
                            (inner-callback *world*)))])))


(define world-gui:text-field% 
  (class* text-field% (world-gui<%>)
    (inherit get-value set-value)
    (init-field inner-callback)
    
    (define/public (compatible? an-elt)
      (text-field-elt? an-elt))
    
    (define/public (update-with! an-elt)
      (unless (string=? (text-field-elt-s an-elt) (get-value))
        (set-value (text-field-elt-s an-elt)))
      (unless (eq? inner-callback (text-field-elt-callback an-elt))
        (set! inner-callback (text-field-elt-callback an-elt))))
      
    
    (super-new [callback (lambda (f e)
                           (change-world! (inner-callback *world* (get-value))))])))


(define world-gui:drop-down% 
  (class* choice% (world-gui<%>)
    (init-field inner-callback)
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
      (unless (eq? inner-callback (drop-down-elt-callback an-elt))
        (set! inner-callback (drop-down-elt-callback an-elt))))

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
                  (inner-callback *world* (get-string-selection))))])))



(define world-gui:slider%
  (class* slider% (world-gui<%>)
    (define/public (compatible? an-elt)
      (slider-elt? an-elt))
    (super-new)))



(define world-gui:image%
  (class* editor-canvas% (world-gui<%>)
    (define/public (compatible? an-elt)
      (image-elt? an-elt))
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
          [inner-callback callback]
          [enabled enabled?])]
    
    [(struct text-field-elt (v callback))
     (new world-gui:text-field% 
          [label #f]
          [parent a-container]
          [init-value v]
          [inner-callback callback])]
    
    [(struct drop-down-elt (val choices callback))
     (new world-gui:drop-down% 
          [label #f]
          [choices choices]
          [selection (list-index (lambda (x) 
                                   (string=? x val))
                                 choices)]
          [parent a-container]
          [inner-callback callback])]
    
    [(struct slider-elt (val min max callback))
     (new world-gui:slider% 
          [label #f]
          [parent a-container]
          [min-value min]
          [max-value max]
          [init-value val]
          [callback (lambda (s e)
                      (change-world! 
                       (callback *world* (send s get-value))))])]
    
    [(struct image-elt (an-image-snip))
     (let* ([pasteboard (new pasteboard%)]
            [canvas (new world-gui:image%
                         [parent a-container]
                         [editor pasteboard])])
       (send pasteboard insert (send an-image-snip copy))
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
                (make-string-elt elt)]
               [(is-a? elt cache-image-snip%)
                (make-image-elt elt)]
               [else
                elt]))
       elts))

;; make-button: string (world -> world) [enabled? boolean] -> element
(define (make-button label callback [enabled? #t])
  (make-button-elt label callback enabled?))

;; make-row: element+ -> element
(define (make-row first-elt . rest-elts)
  (make-row-elt (coerse-primitive-types-to-elts (cons first-elt rest-elts))))

;; make-column: element+ -> element
(define (make-column first-elt . rest-elts)
  (make-column-elt (coerse-primitive-types-to-elts (cons first-elt rest-elts))))

;; make-drop-down: string (listof string) (world string -> world) -> element
(define (make-drop-down default-value choices 
                        [callback 
                         (lambda (old-world a-str)
                           a-str)])
  (unless (member default-value choices)
    (error 'make-drop-down "Value ~s not in the choices ~s" default-value choices))
  (make-drop-down-elt default-value choices callback))

;; make-text-field: string (world string -> world) -> element
(define (make-text-field default-value 
                         [callback
                          (lambda (world a-string)
                            a-string)])
  (make-text-field-elt default-value callback))

;; make-slider: number number number (world number -> world) -> element
(define (make-slider v min max 
                     [callback
                      (lambda (world a-num)
                        a-num)])
  (make-slider-elt v min max callback))







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




(provide 
 
 big-bang
 on-redraw
 
 (rename-out [-make-form make-form])
 make-button
 make-row
 make-column
 make-drop-down
 make-text-field
 make-slider
 
 random-choice
 define-updaters)