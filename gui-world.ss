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
         (only-in srfi/1 list-index)

         "gui-struct.ss"
         "world-support.ss"
         
         )


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
(define *gui* #f)
(define *frame* #f)
(define *eventspace* #f)

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





;; big-bang: world gui -> void
;; Shows the frame, creates the initial world.
(define (big-bang initial-world a-gui)
  (set! *world* initial-world)
  (set! *gui* a-gui)
  (set! *eventspace* (current-eventspace))
  (set-and-show-frame)
  (change-world/f! (lambda (a-world)
                     initial-world)))


;; set-and-show-frame: -> void
(define (set-and-show-frame)
  (set! *frame* (new world-gui:frame% 
                     [label ""] 
                     [width *width*]
                     [height *height*]))
  (send *frame* show #t))



;; on-redraw: (world -> gui) -> void
;; Initializes the redrawing callback.
(define (on-redraw callback)
  (set! *on-redraw-callback* callback)
  (refresh!))



(define (on-tick freq callback)
  (set! *on-tick-frequency* freq)
  (set! *on-tick-callback* callback)
  ;; FIXME: maybe I should use a timer% instead of a thread?
  (thread (lambda ()
            (let loop ()
              (sleep *on-tick-frequency*)
              ;; We run this at low priority, to avoid fighting
              ;; gui callbacks.
              (parameterize ([current-eventspace *eventspace*])
                (queue-callback 
                 (lambda ()
                   (change-world/f! (lambda (a-world)
                                      (*on-tick-callback* a-world))))
                 #f))
              (loop))))
  (void))


(define world-sema (make-semaphore 1))
;; change-world!: world -> void
(define (change-world/f! new-world-f)
  (call-with-semaphore 
   world-sema
   (lambda ()
     (set! *world* (new-world-f *world*))
     (refresh!))))


(define (refresh!)
  (when *on-redraw-callback*
    (let ([new-gui (*on-redraw-callback* *world*)])
      (unless (equal? *gui* new-gui)
        (set! *gui* new-gui)
        (render-gui-to-frame new-gui *frame*)))))


;; render-gui-to-frame: gui frame -> void
;; Clears out the contents of the frame, and adds the gui elements in.
(define (render-gui-to-frame a-gui a-frame)
  ;; Returns true if the gui element has the exact same structure as the
  ;; gui on screen.
  (define (gui-completely-reusable?)
    (match (send a-frame get-children)
      [(list a-column)
       (send a-column compatible? (gui-elt a-gui))]
      [else
       #f]))
  
  (define (render-from-scratch!)
    ;; Remove all children, and add the rendered gui element as a child.
    (send a-frame change-children (lambda (subareas) '()))
    (render-elt! (gui-elt a-gui) a-frame)
    (void))
  
  (define (reuse!)2
    (update-elt! (gui-elt a-gui) 
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


(define (handle-key-event! an-event)
  ...
  #f)





(define world-gui:frame%
  (class frame%
    (super-new)))



(define (on-subwindow-char-mixin super%)
  (class super%
    (define/override (on-subwindow-char receiver event)
      (let ([result
             (super on-subwindow-char receiver event)])
        (cond
          [result result]
          [else
           (handle-key-event! event)])))
    (super-new)))



(define world-gui<%> (interface () 
                       compatible?
                       update-with!))


(define world-gui:row%
  (class* (on-subwindow-char-mixin horizontal-panel%) (world-gui<%>)
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
  (class* (on-subwindow-char-mixin vertical-panel%) (world-gui<%>)
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
  (class* (on-subwindow-char-mixin message%) (world-gui<%>)
    (inherit get-label set-label)
    
    (define/public (compatible? an-elt)
      (string-elt? an-elt))
    
    (define/public (update-with! an-elt)
      (unless (string=? (string-elt-s an-elt) (get-label))
        (set-label (string-elt-s an-elt))))
        
    (super-new [auto-resize #t])))


(define world-gui:button%
  (class* (on-subwindow-char-mixin button%) (world-gui<%>)
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
                           (change-world/f!
                            (lambda (a-world)
                              (world-callback a-world))))])))


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

    (define/override (on-subwindow-char receiver event)
      (super on-subwindow-char receiver event))
    
    (super-new [callback (lambda (f e)
                           (change-world/f!
                            (lambda (a-world)
                              (world-callback a-world (get-value)))))])))


(define world-gui:drop-down% 
  (class* (on-subwindow-char-mixin choice%) (world-gui<%>)
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
                 (change-world/f!
                  (lambda (a-world)
                    (world-callback a-world (get-string-selection)))))])))



(define world-gui:slider%
  (class* (on-subwindow-char-mixin slider%) (world-gui<%>)
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
                 (change-world/f!
                  (lambda (a-world)
                    (world-callback a-world (get-value)))))])))



(define world-gui:image%
  (class* editor-canvas% (world-gui<%>)
    (inherit get-editor min-width min-height)
    
    (define/override (on-char evt)
      (handle-key-event! evt)
      (void))
    
    (define/override (on-event evt)
      (void))
    
    (define/public (compatible? an-elt)
      (image-elt? an-elt))
    
    (define/public (update-with! an-elt)
      (let ([editor (get-editor)]
            [img (send (image-elt-img an-elt) copy)])
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
            (new world-gui:row% 
                 [parent a-container]
                 [stretchable-width #f]
                 [stretchable-height #f])])
       (for ([sub-elt elts])
         (render-elt! sub-elt row-container))
       row-container)]
    
    [(struct column-elt (elts))
     (let ([column-container
            (new world-gui:column% 
                 [parent a-container]
                 [stretchable-width #f]
                 [stretchable-height #f])])
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
                         [horizontal-inset INSET]
                         [vertical-inset INSET]
                         [horiz-margin 0]
                         [vert-margin 0]
                         [stretchable-width #f]
                         [stretchable-height #f]
                         [style '(no-hscroll no-vscroll)]
                         [editor pasteboard])])
       (send canvas min-client-width (+ (image-width img-snip) INSET INSET))
       (send canvas min-client-height (+ (image-height img-snip) INSET INSET))
       (send pasteboard insert img-snip 0 0)
       (send pasteboard set-cursor (make-object cursor% 'arrow))
       canvas)]))

(define INSET 5)



;; update-elt!: elt world-gui<%> -> void
;; Adds an elt to the gui container. 
(define (update-elt! an-elt gui-elt)
  (send gui-elt update-with! an-elt))




;; make-gui: element+ -> gui
(define (-make-gui first-elt . rest-elements)
  (make-gui 
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










(provide 
 
 big-bang
 on-redraw
 on-tick
  

 
 
 
 )
