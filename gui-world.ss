#lang scheme/base

(require scheme/gui/base
         scheme/match
         scheme/class
         scheme/list
         scheme/bool
         htdp/image
         (only-in lang/htdp-beginner image?)
         (only-in srfi/1 list-index)

         "gui-struct.ss"
         "world-support.ss")


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



(define world-sema (make-semaphore 1))
;; change-world!: (-> world) -> void
;; Changes the world.  Ensures that only one thing at a time
;; will be able to get into the critical region.
(define (change-world/f! new-world-f)
  (call-with-semaphore 
   world-sema
   (lambda ()
     (set! *world* (new-world-f *world*))
     (refresh-widgets!))))



(define (handle-key-event! an-event)
  ...
  #f)


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



;; big-bang: world gui -> void
;; Shows the frame, creates the initial world.
(define (big-bang initial-world a-gui)
  (set! *world* initial-world)
  (set! *gui* a-gui)
  (set! *eventspace* (current-eventspace))
  (set! *frame* (new world-gui:frame% 
                     [label ""]))
  (render-elt! *gui* *frame*)
  (send *frame* show #t)
  (change-world/f! (lambda (a-world)
                     initial-world)))


;; refresh-widgets!: -> void
;; Update the widgets in the frame with the new contents in the world.
(define (refresh-widgets!)
  (let ([top-widget (first (send *frame* get-children))])
    (send top-widget update-with! *gui*)))




;; render-elt!: elt container% -> world-gui<%>
;; Consumes an elt, and produces a world-gui<%> widget that's installed in a-container. 
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
    
    [(struct string-elt (s-f))
     (new world-gui:string% [label (s-f *world*)]
          [parent a-container])]
    
    [(struct button-elt (label-f callback enabled?-f))
     (new world-gui:button% 
          [label (label-f *world*)]
          [parent a-container]
          [world-callback callback]
          [enabled (enabled?-f *world*)])]
    
    [(struct text-field-elt (v-f callback enabled?-f))
     (new world-gui:text-field% 
          [label #f]
          [parent a-container]
          [init-value (v-f *world*)]
          [enabled (enabled?-f *world*)]
          [world-callback callback])]
    
    [(struct drop-down-elt (val-f choices-f callback enabled?-f))
     (let ([val (val-f *world*)]
           [choices (choices-f *world*)])
       (new world-gui:drop-down% 
            [label #f]
            [choices choices]
            [selection (list-index (lambda (x) 
                                     (string=? x val))
                                   choices)]
            [enabled (enabled?-f *world*)]
            [parent a-container]
            [world-callback callback]))]
    
    [(struct slider-elt (val-f min-f max-f callback enabled?-f))
     (new world-gui:slider% 
          [label #f]
          [parent a-container]
          [min-value (min-f *world*)]
          [max-value (max-f *world*)]
          [init-value (val-f *world*)]
          [enabled (enabled?-f *world*)]
          [world-callback callback])]
    
    [(struct scene-elt (an-image-snip-f))
     (let* ([pasteboard (new pasteboard%)]
            [img-snip (send (an-image-snip-f *world*) copy)]
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
                       update-with!))


(define world-gui:row%
  (class* (on-subwindow-char-mixin horizontal-panel%) (world-gui<%>)
    (inherit get-children)
    
    (define/public (update-with! an-elt)
      (for-each (lambda (sub-elt sub-gui-elt)
                  (send sub-gui-elt update-with! sub-elt))
                (row-elt-elts an-elt)
                (get-children)))
    
    (super-new)))


(define world-gui:column%
  (class* (on-subwindow-char-mixin vertical-panel%) (world-gui<%>)
    (inherit get-children)
        
    (define/public (update-with! an-elt)
      (for-each (lambda (sub-elt sub-gui-elt)
                  (send sub-gui-elt update-with! sub-elt))
                (column-elt-elts an-elt)
                (get-children)))
        
    (super-new)))


(define world-gui:string% 
  (class* (on-subwindow-char-mixin message%) (world-gui<%>)
    (inherit get-label set-label)
        
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
    (inherit get-value set-value is-enabled? enable)
    (init-field world-callback)
        
    (define/public (update-with! an-elt)
      (match an-elt
        [(struct text-field-elt (val-f callback enabled?-f))
         (let ([new-text (val-f *world*)]
               [new-enabled? (enabled?-f *world*)])
           (unless (string=? new-text (get-value))
             (set-value new-text))
           (unless (boolean=? (is-enabled?) new-enabled?)
             (enable new-enabled?)))]))

    (define/override (on-subwindow-char receiver event)
      (super on-subwindow-char receiver event))
    
    (super-new [callback (lambda (f e)
                           (change-world/f!
                            (lambda (a-world)
                              (world-callback a-world (get-value)))))])))


(define world-gui:drop-down% 
  (class* (on-subwindow-char-mixin choice%) (world-gui<%>)
    (init-field world-callback)
    (inherit get-string-selection get-number get-string clear append set-selection
             is-enabled? enable)
    
    (define/public (update-with! an-elt)
      (match an-elt
        [(struct drop-down-elt (val-f choices-f callback enabled?-f))
         (let ([new-val (val-f *world*)]
               [new-choices (choices-f *world*)]
               [new-enabled? (enabled?-f *world*)])
           
           (unless (andmap string=? (get-choices) new-choices)
             (clear)
             (for ([choice new-choices])
               (append choice)))
           
           (unless (string=? (get-string-selection) new-val)
             (set-selection (list-index (lambda (x) 
                                          (string=? x new-val))
                                        new-choices)))
           
           (unless (boolean=? (is-enabled?) new-enabled?)
             (enable new-enabled?)))]))
    
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
    (inherit get-value set-value
             is-enabled? enable)
    (init min-value
          max-value)
    (init-field world-callback)
        
    (define/public (update-with! an-elt)
      (match an-elt
        [(struct slider-elt (val-f min-f max-f callback enabled?-f))
         (let ([new-val (val-f *world*)]
               [new-enabled? (enabled?-f *world*)])
           ;; fixme: handle changes to min/max ranges
           (unless (= new-val (get-value))
             (set-value new-val))
           
           (unless (boolean=? (is-enabled?) new-enabled?)
             (enable new-enabled?))
           )]))
    
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



(provide big-bang

         ;; Widgets
         row
         col
         message
         button
         drop-down
         text-field
         
         ;; Other helpers
         define-updaters
         random-choice
         #;on-tick)
