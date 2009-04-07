#lang scheme/base

(require scheme/gui/base
         scheme/match
         scheme/class
         scheme/list
         scheme/bool
         scheme/contract
         htdp/image
         (only-in lang/htdp-beginner image?)
         (only-in srfi/1 list-index)
         
         "gui-struct.ss"
         "world-support.ss")


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

(define current-world (make-parameter #f))
(define current-gui-world-eventspace (make-parameter (make-eventspace)))
(define current-top-window (make-parameter #f))
(define current-top-panel (make-parameter #f))
(define current-world-listeners (make-parameter '()))
(define current-stopped? (make-parameter (box #t)))
(define current-stop-when (make-parameter #f))
(define current-last-world-ch (make-parameter (make-channel)))
(define current-on-key-event-callback (make-parameter #f))



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


;; queue-on-world-thread: (-> void) -> void
;; Adds something for the current-gui-world-eventspace's thread to evaluate.
(define (queue-on-world-thread f)
  (parameterize ([current-eventspace (current-gui-world-eventspace)])
    (queue-callback f)))

(define (queue-on-eventspace es f)
  (parameterize ([current-eventspace es])
    (queue-callback f)))



;; change-world!: (-> world) -> void
;; Changes the world.
(define (change-world/f! new-world-f)
  (queue-on-world-thread
   (lambda ()
     (with-handlers ([void (lambda (exn)
                             (set-box! (current-stopped?) #t)
                             (raise exn))])
       (current-world (new-world-f (current-world)))
       (for ([listener (current-world-listeners)])
         (listener (current-world)))
       (when (and (current-stop-when) ((current-stop-when) (current-world)))
         (set-box! (current-stopped?) #t)
         (thread (lambda () (channel-put (current-last-world-ch) (current-world)))))))))



;; handle-key-event!: key-event -> void
(define (handle-key-event! a-key-event)
  (queue-on-world-thread
   (lambda ()
     (when (current-on-key-event-callback)
       (change-world/f! (lambda (a-world)
                          ((current-on-key-event-callback) a-world 
                                                           (send a-key-event get-key-code))))))))


;; stop-when: (world -> boolean) -> void
;; Register a handler that tells when we should stop running a big-bang.
(define (stop-when callback)
  (lambda ()
    (current-stop-when callback)))


;; on-key: (world key -> world) -> (-> void)
;; Register an on-key handler.
(define (on-key callback)
  (lambda ()
    (current-on-key-event-callback callback)))


;; on-tick: number (world -> world) -> (-> void)
;; Register an on-tick handler.
(define (on-tick freq callback)
  (lambda ()
    (thread (lambda ()
              (define (new-alarm-evt)
                (alarm-evt (+ (current-inexact-milliseconds) (* 1000 freq))))
              
              (let loop ([an-alarm-evt (new-alarm-evt)])
                (yield  
                 (handle-evt an-alarm-evt 
                             (lambda (_)
                               (queue-on-world-thread
                                (lambda ()
                                  (change-world/f! (lambda (a-world)
                                                     (callback a-world)))))
                               (when (not (unbox (current-stopped?)))
                                 (loop (new-alarm-evt)))))))))))
   




;; big-bang: world gui -> world
;; Shows the frame, creates the initial world.  When the frame
;; is closed or stop-when is evaluated, returns the last world.
(define (big-bang initial-world a-gui
                  #:dialog? (dialog? #f)
                  #:on-world-change (on-world-change (lambda (a-world) (void)))
                  . registry-hooks)
  (define (run es ch #:make-window make-window)
    (parameterize ([current-eventspace es])
      (queue-callback (lambda ()
                        (current-stopped? (box #f))
                          (current-last-world-ch ch)
                          (current-world initial-world)
                          (current-gui-world-eventspace es)
                          (let* ([window (make-window)]
                                 [top-panel (new world-gui:top-panel% [parent window])])
                            (current-top-window window)
                            (current-top-panel top-panel)
                            (add-listener! on-world-change)
                            (add-listener! (lambda (w)
                                             (refresh-widgets! w a-gui top-panel)))
                            
                            (render-elt! a-gui top-panel es)
                            (change-world/f! (lambda (a-world)
                                               initial-world))
                            
                            (for-each (lambda (t) (t)) registry-hooks)
                            ;; WARNING: the following must be last, 
                            ;; to avoid conflict with the dialog's modal behavior.
                            ;; This will immediately yield if the window is a dialog.
                            (send window show #t))))))
  (let ([es (make-eventspace)]
        [ch (make-channel)])
    (cond 
      ;; Fresh run
      [(not (current-top-panel))
       (run es ch #:make-window (lambda () 
                                  (new (if dialog? world-gui:dialog% world-gui:frame%)
                                       [label ""])))
       (let ([result (yield ch)])
         result)]
      
      ;; Reentrancy
      [else
       ;; hide the old top
       (send (current-top-window) change-children (lambda (x) empty))
       (run es ch #:make-window (lambda () (current-top-window)))
       (let ([result (yield ch)])
         (send (current-top-window) change-children (lambda (x) empty))
         (send (current-top-window) add-child (current-top-panel))
         result)])))



;; add-listener!: (world -> void) -> void
;; Adds a listener that will react when the world changes.
(define (add-listener! a-listener)
  (queue-on-world-thread
   (lambda ()
     (current-world-listeners (cons a-listener (current-world-listeners))))))


;; refresh-widgets!: world gui panel -> void
;; Update the widgets in the frame with the new contents in the world.
(define (refresh-widgets! a-world a-gui a-panel)
  (let ([top-widget (first 
                     (filter (lambda (x) (is-a? x world-gui<%>))
                             (send a-panel get-children)))])
    (dynamic-wind (lambda ()
                    (send a-panel begin-container-sequence))
                  (lambda ()
                    (send top-widget update-with! a-gui))
                  (lambda ()
                    (send a-panel end-container-sequence)))))




;; render-elt!: elt container% eventspace -> world-gui<%>
;; Consumes an elt, and produces a world-gui<%> widget that's installed in a-container.
;; GUI Events that occur should be run in the context of the given eventspace es.
(define (render-elt! an-elt a-container an-eventspace)
  (match an-elt
    [(struct row-elt (elts))
     (let ([row-container 
            (new world-gui:row% 
                 [parent a-container]
                 [stretchable-width #f]
                 [stretchable-height #f]
                 [eventspace an-eventspace])])
       (for ([sub-elt elts])
         (render-elt! sub-elt row-container an-eventspace))
       row-container)]
    
    [(struct column-elt (elts))
     (let ([column-container
            (new world-gui:column% 
                 [parent a-container]
                 [stretchable-width #f]
                 [stretchable-height #f]
                 [eventspace an-eventspace])])
       (for ([sub-elt elts])
         (render-elt! sub-elt column-container an-eventspace))
       column-container)]
    
    [(struct box-group-elt (label-f sub-elt enabled?-f))
     (let ([a-group-box
            (new world-gui:group-box%
                 [parent a-container]
                 [label (displayable->string (label-f (current-world)))]
                 [enabled (enabled?-f (current-world))]
                 [eventspace an-eventspace])])
       (render-elt! sub-elt a-group-box an-eventspace)
       a-group-box)]
    
    [(struct displayable-elt (s-f))
     (new world-gui:string% 
          [label 
           (displayable->string (s-f (current-world)))]
          [parent a-container]
          [eventspace an-eventspace])]
    
    [(struct button-elt (label-f callback enabled?-f))
     (new world-gui:button% 
          [label (displayable->string (label-f (current-world)))]
          [parent a-container]
          [world-callback callback]
          [enabled (enabled?-f (current-world))]
          [eventspace an-eventspace])]
    
    [(struct text-field-elt (v-f callback enabled?-f))
     (new world-gui:text-field% 
          [label #f]
          [parent a-container]
          [init-value (displayable->string (v-f (current-world)))]
          [enabled (enabled?-f (current-world))]
          [world-callback callback]
          [eventspace an-eventspace])]
    
    [(struct drop-down-elt (val-f choices-f callback enabled?-f))
     (let ([val (displayable->string (val-f (current-world)))]
           [choices (map displayable->string (choices-f (current-world)))])
       (new world-gui:drop-down% 
            [label #f]
            [choices choices]
            [selection (list-index (lambda (x) 
                                     (string=? x val))
                                   choices)]
            [enabled (enabled?-f (current-world))]
            [parent a-container]
            [world-callback callback]
            [eventspace an-eventspace]))]
    
    [(struct slider-elt (val-f min-f max-f callback enabled?-f))
     (new world-gui:slider% 
          [label #f]
          [parent a-container]
          [min-value (min-f (current-world))]
          [max-value (max-f (current-world))]
          [init-value (val-f (current-world))]
          [enabled (enabled?-f (current-world))]
          [world-callback callback]
          [eventspace an-eventspace])]
    
    [(struct checkbox-elt (label-f val-f callback enabled?-f))
     (new world-gui:checkbox%
          [label (displayable->string (label-f (current-world)))]
          [parent a-container]
          [value (val-f (current-world))]
          [enabled (enabled?-f (current-world))]
          [world-callback callback]
          [eventspace an-eventspace])]
    
    [(struct canvas-elt (an-image-snip-f callback))
     (let* ([pasteboard (new pasteboard%)]
            [img-snip (send (an-image-snip-f (current-world)) copy)]
            [canvas (new world-gui:canvas%
                         [parent a-container]
                         [world-callback callback]
                         [min-width (image-width img-snip)]
                         [min-height (image-height img-snip)]
                         [horizontal-inset INSET]
                         [vertical-inset INSET]
                         [horiz-margin 0]
                         [vert-margin 0]
                         [stretchable-width #f]
                         [stretchable-height #f]
                         [style '(no-hscroll no-vscroll)]
                         [editor pasteboard]
                         [eventspace an-eventspace])])
       (send canvas min-client-width (+ (image-width img-snip) INSET INSET))
       (send canvas min-client-height (+ (image-height img-snip) INSET INSET))
       (send pasteboard insert img-snip 0 0)
       (send pasteboard set-cursor (make-object cursor% 'arrow))
       canvas)]))

(define INSET 5)


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



(define world-gui:frame%
  (class frame% #;(on-subwindow-char-mixin frame%)
    (define/augment (on-close)
      (inner (void) on-close)
      (set-box! (current-stopped?) #t)
      (thread (lambda () (channel-put (current-last-world-ch) (current-world)))))
    (super-new)))

(define world-gui:dialog%
  (class dialog% #;(on-subwindow-char-mixin dialog%)
    (define/augment (on-close)
      (inner (void) on-close)
      (set-box! (current-stopped?) #t)
      (thread (lambda () (channel-put (current-last-world-ch) (current-world)))))
    (super-new)
    (new button% 
         [parent this]
         [label "Close"]
         [callback (lambda (b e)
                     (send this show #f))])))


(define world-gui:top-panel%
  (class horizontal-panel%
    (super-new)))



(define world-gui<%> (interface () 
                       update-with!))


(define world-gui:row%
  (class* horizontal-panel% #;(on-subwindow-char-mixin horizontal-panel%) (world-gui<%>)
    (inherit get-children)
    (init-field eventspace)
    
    (define/public (update-with! an-elt)
      (for-each (lambda (sub-elt sub-gui-elt)
                  (send sub-gui-elt update-with! sub-elt))
                (row-elt-elts an-elt)
                (get-children)))
    
    (super-new)))


(define world-gui:column%
  (class* vertical-panel% #;(on-subwindow-char-mixin vertical-panel%) (world-gui<%>)
    (inherit get-children)
    (init-field eventspace)
    
    (define/public (update-with! an-elt)
      (for-each (lambda (sub-elt sub-gui-elt)
                  (send sub-gui-elt update-with! sub-elt))
                (column-elt-elts an-elt)
                (get-children)))
    
    (super-new)))


(define world-gui:group-box%
  (class* group-box-panel% #;(on-subwindow-char-mixin group-box-panel%) (world-gui<%>)
    (inherit get-children get-label set-label is-enabled? enable)
    (init-field eventspace)
    
    (define/public (update-with! an-elt)
      (queue-on-eventspace eventspace
                           (lambda ()
                             (match an-elt
                               [(struct box-group-elt (val-f sub-elt enabled?-f))
                                (let ([new-val (displayable->string (val-f (current-world)))]
                                      [new-enabled? (enabled?-f (current-world))])
                                  (unless (string=? new-val (get-label))
                                    (set-label new-val))
                                  (unless (boolean=? new-enabled? (is-enabled?))
                                    (enable new-enabled?))
                                  
                                  (send (first (get-children)) update-with! sub-elt))]))))
    
    (super-new)))


(define world-gui:string% 
  (class* (on-subwindow-char-mixin message%) (world-gui<%>)
    (inherit get-label set-label)
    (init-field eventspace)
    
    (define/public (update-with! an-elt)
      (queue-on-eventspace eventspace
                           (lambda ()
                             (match an-elt 
                               [(struct displayable-elt (val-f))
                                (let ([a-str (displayable->string (val-f (current-world)))])
                                  (unless (string=? a-str (get-label))
                                    (set-label a-str)))]))))
         
    (super-new [auto-resize #t])))


(define world-gui:button%
  (class* (on-subwindow-char-mixin button%) (world-gui<%>)
    (inherit get-label is-enabled? enable
             min-width min-height
             vert-margin horiz-margin)
    
    (init-field world-callback)
    (init-field eventspace)
    
    (define/public (update-with! an-elt)
      (queue-on-eventspace eventspace 
                           (lambda ()
                             (match an-elt
                               [(struct button-elt (val-f callback enabled?-f))
                                (let ([new-val (displayable->string (val-f (current-world)))]
                                      [new-enabled? (enabled?-f (current-world))])
                                  (unless (string=? new-val (get-label))
                                    (set-label new-val))
                                  (unless (boolean=? (is-enabled?) new-enabled?)
                                    (enable new-enabled?)))]))))
         
    ;; set-label: string -> void
    ;; Sets the label, but also auto-resizes based on the label's size.
    (define/override (set-label new-label)
      (super set-label new-label)
      (auto-resize))
    
    
    ;; auto-resize: -> void
    ;; Automatically resize the button to fit the label.
    (define (auto-resize)
      (let ([s (get-label)])
        (let-values ([(mw mh) (get-window-text-extent s normal-control-font #t)])
          (min-width (+ dx mw))
          (min-height (+ dy mh)))))
    
    
    (super-new [callback (lambda (b e)
                           (queue-on-eventspace eventspace
                                                (lambda ()
                                                  (change-world/f!
                                                   (lambda (a-world)
                                                     (world-callback a-world))))))])
    
    ;; We record the old space-padding values around the button's label.  For some
    ;; reason, using horiz-margin and vert-margin isn't correct, but I don't
    ;; know why.  dx and dy are only used with regard to auto-resize above.
    (define-values (dx dy)
      (let-values ([(mw mh) (get-window-text-extent (get-label) normal-control-font #t)])
        (values (- (min-width) mw)
                (- (min-height) mh))))))


(define world-gui:text-field% 
  (class* text-field% (world-gui<%>)
    (inherit get-value set-value is-enabled? enable min-width min-height)
    (init-field world-callback)
    (init-field eventspace)
    
    (define/public (update-with! an-elt)
      (queue-on-eventspace eventspace
                           (lambda ()
                             (match an-elt
                               [(struct text-field-elt (val-f callback enabled?-f))
                                (let ([new-text (displayable->string (val-f (current-world)))]
                                      [new-enabled? (enabled?-f (current-world))])
                                  (unless (string=? new-text (get-value))
                                    (set-value new-text))
                                  (unless (boolean=? (is-enabled?) new-enabled?)
                                    (enable new-enabled?)))
                                (auto-resize)]))))
    
    (define/override (on-subwindow-char receiver event)
      (super on-subwindow-char receiver event))
    
    
    ;; auto-resize: -> void
    ;; Automatically resize the button to fit the label.
    (define (auto-resize)
      (let ([s (get-value)])
        (let-values ([(mw mh) (get-window-text-extent s normal-control-font #t)])
          (min-width (+ dx mw))
          (min-height (+ dy mh)))))
    
    (super-new [callback (lambda (f e)
                           (queue-on-eventspace eventspace 
                                                (lambda ()
                                                  (change-world/f!
                                                   (lambda (a-world)
                                                     (world-callback a-world (get-value)))))))])
    
    
     ;; We record the old space-padding values around the button's label.  For some
    ;; reason, using horiz-margin and vert-margin isn't correct, but I don't
    ;; know why.  dx and dy are only used with regard to auto-resize above.
    (define-values (dx dy)
      (let-values ([(mw mh) (get-window-text-extent (get-value) normal-control-font #t)])
        (values (- (min-width) mw)
                (- (min-height) mh))))))


(define world-gui:drop-down% 
  (class* choice% #;(on-subwindow-char-mixin choice%) (world-gui<%>)
    (init-field world-callback)
    (inherit get-string-selection get-number get-string clear append 
             set-selection
             is-enabled? enable)
    
    ;; TRICKY:
    ;; We need to keep track of some internal state of the drop down.
    ;; On Windows, as the user is selecting a new string, the string selection
    ;; switches.  This interferes if an on-tick event happens, because then
    ;; the world will snap the selection back to the world state.
    ;; internal-selection-string maintains the last selection that was
    ;; chosen by a control event; see the callback for the set!.
    (define internal-selection-string "")
    
    (define/public (update-with! an-elt)
      (match an-elt
        [(struct drop-down-elt (val-f choices-f callback enabled?-f))
         (let ([new-val (displayable->string (val-f (current-world)))]
               [new-choices (map displayable->string (choices-f (current-world)))]
               [new-enabled? (enabled?-f (current-world))])
           
           (unless (and (= (length (get-choices))
                           (length new-choices))
                        (andmap string=? (get-choices) new-choices))
             (clear)
             (for ([choice new-choices])
               (append choice))
             (set-selection (or (list-index
                                 (lambda (x) 
                                   (string=? x internal-selection-string))
                                 new-choices)
                                0))
             (queue-callback (lambda () (internal-callback))))
           
           (unless (string=? internal-selection-string new-val)
             (set-selection (list-index (lambda (x) 
                                          (string=? x new-val))
                                        new-choices))
             (set! internal-selection-string new-val))
           
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
    
    
    (define (internal-callback)
      (set! internal-selection-string (get-string-selection))
      (change-world/f!
       (lambda (a-world)
         (world-callback 
          a-world (get-string-selection)))))
    (super-new
     [callback (lambda (c e) (internal-callback))])))


;; clamp: number number number -> number
(define (clamp x low high)
  (min (max x low) high))


;; Slightly special: the slider is really held in an inner widget.  We do this
;; because we may need to replace the whole widget on update time, and we want to
;; do this without disrupting parent widgets.
;;
;; There's also some special logic to ensure the value's between the min and max,
;; and to propagate such clamping into the world if necessary.  Slightly fragile.
(define world-gui:slider%
  (class* horizontal-panel% (world-gui<%>)
    
    (init-field min-value max-value world-callback)
    (init init-value label)
    (inherit delete-child)
    
    ;; We maintain an inner class that represents the real slider.
    (define inner-slider%
      (class* (on-subwindow-char-mixin slider%) ()
        (inherit get-value set-value
                 is-enabled? enable)
        (init-field min-value max-value world-callback)
        
        (define/public (get-min-value)
          min-value)
        (define/public (get-max-value)
          max-value)
        
        (super-new
         [min-value min-value]
         [max-value max-value]
         [callback (lambda (s e)
                     (change-world/f!
                      (lambda (a-world)
                        (world-callback a-world (get-value)))))])))
    
    (define/public (update-with! an-elt)
      (match an-elt
        [(struct slider-elt (val-f min-f max-f callback enabled?-f))
         (let* ([new-min (min-f (current-world))]
                [new-max (max-f (current-world))]
                [new-val (clamp (val-f (current-world)) new-min new-max)]
                [new-enabled? (enabled?-f (current-world))])
           
           (unless (and (= new-min (send inner-slider get-min-value))
                        (= new-max (send inner-slider get-max-value)))
             (delete-child inner-slider)
             (set! inner-slider (make-inner-slider new-min new-max new-val))
             (queue-callback (lambda () 
                               (change-world/f!
                                (lambda (a-world)
                                  (world-callback a-world (send inner-slider get-value)))))))
           
           (unless (= new-val (send inner-slider get-value))
             (send inner-slider set-value new-val))
           
           (unless (boolean=? (send inner-slider is-enabled?) new-enabled?)
             (send inner-slider enable new-enabled?)))]))
    
    
    ;; make-inner-slider: number number number -> inner-slider%
    ;; Makes the inner slider.
    (define (make-inner-slider min-value max-value init-value)
      (new inner-slider% 
           [parent this]
           [world-callback world-callback]
           [min-value min-value]
           [max-value max-value]
           [init-value (clamp init-value min-value max-value)]
           [min-width 50]
           [label label]))
    
    (super-new)
    (define inner-slider (make-inner-slider min-value max-value init-value))))


(define world-gui:checkbox%
  (class* check-box% #;(on-subwindow-char-mixin check-box%) (world-gui<%>)
    (init-field world-callback)
    (inherit get-value set-value get-label is-enabled? enable min-width min-height)
    
    (define/public (update-with! an-elt)
      (match an-elt
        [(struct checkbox-elt (label-f val-f callback enabled?-f))
         (let ([new-label (displayable->string (label-f (current-world)))]
               [new-val (val-f (current-world))]
               [new-enabled? (enabled?-f (current-world))])
           (unless (string=? new-label (get-label))
             (set-label new-label))
           
           (unless (boolean=? new-val (get-value))
             (set-value new-val))
           
           (unless (boolean=? (is-enabled?) new-enabled?)
             (enable new-enabled?)))]))
    
    
    (define/override (set-label a-label)
      (super set-label a-label)
      (auto-resize))
    
    ;; auto-resize: -> void
    ;; Automatically resize the button to fit the label.
    (define (auto-resize)
      (let ([s (get-label)])
        (let-values ([(mw mh) (get-window-text-extent s normal-control-font #t)])
          (min-width (+ dx mw))
          (min-height (+ dy mh)))))
    
    
    (super-new
     [callback (lambda (s e)
                 (change-world/f!
                  (lambda (a-world)
                    (world-callback a-world (get-value)))))])
    
    
    ;; We record the old space-padding values around the button's label.  For some
    ;; reason, using horiz-margin and vert-margin isn't correct, but I don't
    ;; know why.  dx and dy are only used with regard to auto-resize above.
    (define-values (dx dy)
      (let-values ([(mw mh) (get-window-text-extent (get-label) normal-control-font #t)])
        (values (- (min-width) mw)
                (- (min-height) mh))))))






(define world-gui:canvas%
  (class* editor-canvas% (world-gui<%>)
    (inherit get-editor min-width min-height)
    
    (init-field world-callback)
    
    (define/override (on-char evt)
      (handle-key-event! evt)
      (void))
    
    (define/override (on-event evt)
      (when (send evt get-left-down)
        (let ([x (send evt get-x)]
              [y (send evt get-y)])
          (change-world/f!
           (lambda (a-world)
             (world-callback a-world x y))))))
    
    
    (define/public (update-with! an-elt)
      (match an-elt
        [(struct canvas-elt (scene-f callback))
         (let ([new-scene (scene-f (current-world))]
               [editor (get-editor)])
           (dynamic-wind 
            (lambda () 
              (send editor begin-edit-sequence))
            (lambda () 
              (send editor erase)
              (let ([snip (send new-scene copy)])
                (send editor insert snip 0 0)
                (let ([new-width (+ (image-width snip) INSET)]
                      [new-height (+ (image-height snip) INSET)])
                  (when (or (not (= (min-width) new-width))
                            (not (= (min-height)  new-height)))
                    (min-width new-width)
                    (min-height new-height)))))
            (lambda () 
              (send editor end-edit-sequence))))]))
    
    (super-new)))


(provide big-bang 
         on-tick 
         on-key
         stop-when
         
         elt? 
         row 
         col
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


(provide ;; Other helpers
 define-updaters
 update
 with-getter/updater
 
 update-color-red update-color-green update-color-blue
 update-posn-x update-posn-y
 color-red-accessor color-green-accessor color-blue-accessor
 posn-x-accessor posn-y-accessor
 
 
 random-choice
 place-image
 empty-scene
 nw:rectangle
 (all-from-out htdp/image))
