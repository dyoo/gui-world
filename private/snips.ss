#lang scheme/base

(require scheme/gui/base
         scheme/match
         scheme/class
         embedded-gui
         "gui-struct.ss"
         "params.ss")

(define ... 'todo)

;; gui-world snips (for the pasteboard)

(define (elt->snip an-element an-eventspace)
  (let* ([pasteboard (new aligned-pasteboard%)]
         [snip (new element-snip% 
                    [element an-element]
                    [editor pasteboard]
                    [eventspace an-eventspace])])
    snip))
    

(define element-snip%
  (class (stretchable-editor-snip-mixin editor-snip%)
    (init-field element)
    (init-field eventspace)
    (define alignment #f)

    (inherit get-editor)

    (define/override (set-admin admin)
      (super set-admin admin)
      ;; When the admin is set, then the alignment should
      ;; have the context to align itself.
      (letrec ([f
                (lambda ()
                  (cond
                    [alignment
                     (void)]
                    [(get-editor)
                     =>
                     (lambda (editor)
                       (set! alignment
                             (elt->alignment element editor eventspace)))]
                    [else
                     (queue-callback f)]))])
      (queue-callback f)))

    (define/override (copy)
      (elt->snip element eventspace))

    (super-new)))




;; elt->snip: elt alignment-parent<%> eventspace -> alignment
(define (elt->alignment an-elt parent an-eventspace)
  (match an-elt
    [(struct row-elt (elts))
     ...
     #;(let ([alignment (new row% [elt an-elt] [parent parent])])
       (for ([sub-elt elts])
         (elt->alignment sub-elt alignment an-eventspace))
       alignment)]

    
    [(struct column-elt (elts))
     ...
     #;(let ([alignment (new col% [elt an-elt] [parent parent])])
       (for ([sub-elt elts])
         (elt->alignment sub-elt alignment an-eventspace))
       alignment)]
    
      
    [(struct box-group-elt (label-f sub-elt enabled?-f))
     ...
     #;(let ([alignment (new box-group% [elt an-elt] [parent parent])])
       (elt->alignment sub-elt alignment an-eventspace)
       alignment)]
    
    [(struct pasteboard-elt (elts))
     (new elt:pasteboard% 
          [elt an-elt]
          [parent parent]
          [eventspace an-eventspace])]

    
    [(struct displayable-elt (s-f))
     (new elt:displayable% 
          [elt an-elt]
          [parent parent]
          [eventspace an-eventspace])]

     
    [(struct button-elt (label-f callback enabled?-f))
     ;; FIXME!
     ...]
    
    [(struct text-field-elt (v-f callback enabled?-f))
     ;; FIXME!
     (new elt:text-field% 
          [an-elt an-elt]
          [parent parent]
          [eventspace an-eventspace])]

    
    [(struct drop-down-elt (val-f choices-f callback enabled?-f))
     ;; FIXME!
     ...]

    [(struct slider-elt (val-f min-f max-f callback enabled?-f))
     ;; FIXME!
     ...]

    [(struct checkbox-elt (label-f val-f callback enabled?-f))
     ;; FIXME!
     ...]
    
    [(struct canvas-elt (an-image-snip-f callback))
     ;; FIXME!
     ...]))



#;(define row% 
  (class horizontal-alignment%
    (init-field elt)
    (init-field parent)
    (super-new [parent parent])))
                                    

#;(define column% 
  (class vertical-alignment%
    (init-field elt)
    (init-field parent)
    (super-new [parent parent])))


#;(define box-group%
  (class vertical-alignment%
    (init-field elt)
    (init-field parent)
    (super-new [parent parent])))


(define elt:pasteboard%
  (class snip-wrapper%
    (init-field elt)
    (init-field eventspace)
    (super-new [snip (new editor-snip% [editor (new pasteboard%)])])))


(define elt:displayable%
  (class snip-wrapper%
    (init-field elt)
    (init-field eventspace)
    (super-new [snip (new editor-snip% [editor (new text%)])])))


(define elt:text-field%
  (class snip-wrapper%
    (init-field elt)
    (init-field eventspace)
    (super-new [snip (new editor-snip% [editor (new text%)])])))



(define (test)
  (let* ([f (new frame% [label ""])]
         [t (new text%)]
         [c (new editor-canvas% 
                 [parent f]
                 [editor t])])
    (send t insert (elt->snip (message "hello")
                              (current-eventspace)))
    (send f show #t)))