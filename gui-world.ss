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
  (initialize-and-populate-frame!)
  (change-world/f! (lambda (a-world)
                     initial-world)))


(define (initialize-and-populate-frame!)
  (set! *frame* (new world-gui:frame% 
                     [label ""]))
  (send *frame* show #t))



(define world-sema (make-semaphore 1))
;; change-world!: (-> world) -> void
;; Changes the world.  Ensures that only one thing at a time
;; will be able to get into the critical region.
(define (change-world/f! new-world-f)
  (call-with-semaphore 
   world-sema
   (lambda ()
     (set! *world* (new-world-f *world*))
     (refresh!))))


(define (refresh!)
  (void)
  #;(when *on-redraw-callback*
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
       (send a-column compatible? a-gui)]
      [else
       #f]))
  
  (define (render-from-scratch!)
    ;; Remove all children, and add the rendered gui element as a child.
    (send a-frame change-children (lambda (subareas) '()))
    (render-elt! a-gui a-frame)
    (void))
  
  (define (reuse!)
    (update-elt! a-gui (first (send a-frame get-children))))
  
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
      (match an-elt 
        [(struct string-elt (val-f))
         (let ([a-str (val-f *world*)])
           (unless (string=? a-str (get-label))
             (set-label a-str)))]))
        
    (super-new [auto-resize #t])))


(define world-gui:button%
  (class* (on-subwindow-char-mixin button%) (world-gui<%>)
    (inherit get-label set-label is-enabled? enable)
    
    (init-field world-callback)
    
    (define/public (compatible? an-elt)
      (button-elt? an-elt))
    
    (define/public (update-with! an-elt)
      (match an-elt
        [(struct button-elt (val-f callback enabled?-f))
         (let ([new-val (val-f *world*)]
               [new-enabled? (enabled?-f *world*)])
           (unless (string=? new-val (get-label))
             (set-label new-val))
           (unless (boolean=? (is-enabled?) new-enabled?)
             (enable new-enabled?)))]))
        
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
      (match an-elt
        [(struct text-field-elt (val-f callback))
         (let ([new-text (val-f *world*)])
           (unless (string=? new-text (get-value))
             (set-value new-text)))]))

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
      (match an-elt
        [(struct drop-down-elt (val-f choices-f callback))
         (let ([new-val (val-f *world*)]
               [new-choices (choices-f *world*)])
           
           (unless (andmap string=? (get-choices) new-choices)
             (clear)
             (for ([choice new-choices])
               (append choice)))
           
           (unless (string=? (get-string-selection) new-val)
             (set-selection (list-index (lambda (x) 
                                          (string=? x new-val))
                                        new-choices))))]))
    
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
      (match an-elt
        [(struct slider-elt (val-f min-f max-f callback))
         (let ([new-val (val-f *world*)])
           ;; fixme: handle changes to min/max ranges
           (unless (= new-val (get-value))
             (set-value new-val)))]))
    
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
      (scene-elt? an-elt))
    
    (define/public (update-with! an-elt)
      (match an-elt
        [(struct scene-elt (scene-f))
         (let ([new-scene (scene-f *world*)]
               [editor (get-editor)])
           (dynamic-wind 
            (lambda () 
              (send editor begin-edit-sequence))
            (lambda () 
              (send editor erase)
              (send editor insert new-scene 0 0))
            (lambda () 
              (send editor end-edit-sequence))))]))

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
    
    [(struct scene-elt (an-image-snip))
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
















#;(define (on-tick freq callback)
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







(provide big-bang
         #;on-tick)
