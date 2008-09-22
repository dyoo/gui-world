#lang scheme/base

(require scheme/gui/base
         scheme/match
         scheme/class
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
(define-struct (text-field-elt elt) (e callback) #:transparent)
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
  ;; Remove all children
  (send a-frame change-children (lambda (subareas) '()))
  ;; Add a new child
  (let ([a-panel (new world-gui:column% [parent a-frame])])
    (render-elt! (form-elt a-form) a-panel)))





(define world-gui<%> (interface () to-elt))

(define world-gui:row%
  (class* horizontal-panel% (world-gui<%>)
    (define/public (to-elt)
      ...)
    (super-new)))

(define world-gui:column%
  (class* vertical-panel% (world-gui<%>)
    (define/public (to-elt)
      ...)
    (super-new)))

(define world-gui:string% 
  (class* message% (world-gui<%>)
    (define/public (to-elt)
      ...)
    (super-new)))

(define world-gui:button%
  (class* button% (world-gui<%>)
    (define/public (to-elt)
      ...)
    (super-new)))

(define world-gui:text-field% 
  (class* text-field% (world-gui<%>)
    (define/public (to-elt)
      ...)
    (super-new)))

(define world-gui:drop-down% 
  (class* choice% (world-gui<%>)
    (define/public (to-elt)
      ...)
    (super-new)))

(define world-gui:slider%
  (class* slider% (world-gui<%>)
    (define/public (to-elt)
      ...)
    (super-new)))

(define world-gui:image%
  (class* editor-canvas% (world-gui<%>)
    (define/public (to-elt)
      ...)
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
          [callback (lambda (b e)
                      (change-world! (callback *world*)))]
          [enabled enabled?])]
    
    [(struct text-field-elt (v callback))
     (new world-gui:text-field% 
          [label #f]
          [parent a-container]
          [init-value v]
          [callback (lambda (t e)
                      (change-world! (callback *world* (send t get-value))))])]
    
    [(struct drop-down-elt (val choices callback))
     (new world-gui:drop-down% 
          [label #f]
          [choices choices]
          [selection (list-index (lambda (x) 
                                   (string=? x val))
                                 choices)]
          [parent a-container]
          [callback (lambda (c e)
                      (change-world! 
                       (callback *world* (send c get-string-selection))))])]
    
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