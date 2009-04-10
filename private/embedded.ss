#lang scheme/base

(require scheme/gui/base
         scheme/match
         scheme/class
         scheme/contract
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

    
    (define/public (refresh a-world a-css)
      (queue-on-eventspace eventspace
                           (lambda () (send alignment refresh a-world a-css))))
    
    
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
    [(struct row-elt (elts css-f))
     (let ([alignment (new elt:row%
                           [elt an-elt] 
                           [parent parent]
                           [eventspace an-eventspace])])
       (for ([sub-elt elts])
           (elt->alignment sub-elt alignment an-eventspace))
       alignment)]
    
    
    [(struct column-elt (elts css-f))
     (let ([alignment (new elt:column%
                           [elt an-elt] 
                           [parent parent]
                           [eventspace an-eventspace])])
       (for ([sub-elt elts])
           (elt->alignment sub-elt alignment an-eventspace))
       alignment)]

    
    [(struct box-group-elt (label-f sub-elt enabled?-f css-f))
     ...
     #;(let ([alignment (new box-group% [elt an-elt] [parent parent])])
       (elt->alignment sub-elt alignment an-eventspace)
       alignment)]
    
    [(struct pasteboard-elt (elts css-f))
     (new elt:pasteboard% 
          [elt an-elt]
          [parent parent]
          [eventspace an-eventspace])]

    
    [(struct displayable-elt (s-f css-f))
     (new elt:displayable% 
          [elt an-elt]
          [parent parent]
          [eventspace an-eventspace])]

     
    [(struct button-elt (label-f callback enabled?-f css-f))
     (new elt:button%
          [elt an-elt]
          [parent parent]
          [eventspace an-eventspace])]
    
    [(struct text-field-elt (v-f callback enabled?-f css-f))
     ;; FIXME!
     (new elt:text-field% 
          [an-elt an-elt]
          [parent parent]
          [eventspace an-eventspace])]

    
    [(struct drop-down-elt (val-f choices-f callback enabled?-f css-f))
     ;; FIXME!
     ...]

    [(struct slider-elt (val-f min-f max-f callback enabled?-f css-f))
     ;; FIXME!
     ...]

    [(struct checkbox-elt (label-f val-f callback enabled?-f css-f))
     ;; FIXME!
     ...]
    
    [(struct canvas-elt (an-image-snip-f callback css-f))
     ;; FIXME!
     ...]))


(define (make-elt:parent% super%)
  (class super%
    (init-field elt)
    (init-field eventspace)

    (define children '())
    
    (define/override (add-child a-child)
      (super add-child a-child)
      (set! children (cons a-child children)))
    
    (define/override (delete-child a-child)
      (super delete-child a-child)
      (set! children (remq a-child children)))
    
    (define/public (refresh a-world a-css)
      (queue-on-eventspace 
       eventspace
       (lambda ()
         (for ([child children])
           (send child refresh (current-world) a-css)))))
    
    (super-new)))

(define elt:row%
  (make-elt:parent% horizontal-alignment%))

(define elt:column%
  (make-elt:parent% vertical-alignment%))



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
    
    (define editor (new text%))
    (define inner-snip (new editor-snip% 
                            [editor editor]
                            [with-border? #f]))

    (send editor insert "uninitialized")
    
    (define/public (refresh a-world a-css)
      (queue-on-eventspace 
       eventspace
       (lambda ()
         (match elt 
           [(struct displayable-elt (val-f css-f))
            (let ([new-label (val-f a-world)])
              (send editor begin-edit-sequence)
              (send editor erase)
              (send editor insert new-label)
              (send editor end-edit-sequence))]))))
                             
    
    (super-new [snip inner-snip])))


(define elt:button%
  (class snip-wrapper%
    (init-field elt)
    (init-field eventspace)
    
    (define editor (new aligned-pasteboard%))
    (define inner-snip (new editor-snip%
                            [editor editor]
                            [with-border? #f]))
    (define button #f)
    

    (define/public (refresh a-world a-css)
      (queue-on-eventspace
       eventspace
       (lambda ()
         (match elt
           [(struct button-elt (val-f callback enabled?-f css-f))
            (send editor begin-edit-sequence)
            (let ([b-label (val-f a-world)]
                  [b-callback (lambda (b e)
                                (change-world/f! (lambda (a-world) (callback a-world))))])
              (when button
                (send editor delete-child button))
              
              (set! button (new embedded-text-button%
                                [parent editor]
                                [label b-label]
                                [callback b-callback])))
            (send editor end-edit-sequence)]))))
    
    (super-new (snip inner-snip))))


(define elt:text-field%
  (class snip-wrapper%
    (init-field elt)
    (init-field eventspace)
    (super-new [snip (new editor-snip% [editor (new text%)])])))


(define (test-1)
  (define initial-world 0)
  (current-gui-world-eventspace (current-eventspace))
  (current-world initial-world)
  (let* ([f (new frame% [label ""])]
         [e (new pasteboard%)]
         [c (new editor-canvas% 
                 [parent f]
                 [editor e])]
         [s1 (elt->snip (message (lambda (world)
                                  (format "hello ~s" world)))
                       (current-eventspace))]
         
         [s2 (elt->snip (button (lambda (world) (format "Press me! (~a)" world))
                                (lambda (world)
                                  (add1 world)))
                        (current-eventspace))])
    (send e insert s1)
    (send e insert s2)
    (send s1 refresh initial-world (make-css))
    (send s2 refresh initial-world (make-css))
    
    (printf "~a children~n" (length (pasteboard-children e)))
    
    (add-listener! (lambda (w)
                     (send s1 refresh w (make-css))
                     (send s2 refresh w (make-css))))
    
    (send f show #t)))

    
(define (pasteboard-children a-pasteboard)
  (let loop ([snip (send a-pasteboard find-first-snip)])
    (cond
      [(not snip)
       '()]
      [else
       (cons snip (loop (send snip next)))])))



(provide/contract [elt->snip (elt? eventspace? . -> . (is-a?/c element-snip%))])
