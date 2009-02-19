#lang scheme/base

(require "../gui-struct.ss"
         "../todo.ss"
         scheme/class
         scheme/match
         scheme/gui/base
         embedded-gui
         (for-syntax scheme/base))





;; Embeddable version of gui-world.  Produces DrScheme snips.
;(define (big-bang a-world a-gui)
;  ...)


;; All of the embedded elements will implement alignment<%>.
(define embed<%> (interface ()
                   get-top))



;;; gui->snip: gui embed<%> -> embed<%>
;;; Translates a gui into a snip that's attached to the given
;;; parent.
(define (gui->snip a-gui parent)
  (match a-gui
    [(struct row-elt (elts))
     (let ([row (new embed:row%
                     [parent parent])])
       (for ([elt elts])
         (gui->snip elt row))
       row)]
    
    [(struct column-elt (elts))
     (let ([col (new embed:col%
                     [parent parent])])
       (for ([elt elts])
         (gui->snip elt col))
       col)]

    [(struct box-group-elt (val-f a-subelt enabled?-f))
     TODO]

    [(struct displayable-elt (val-f))
     (new embed:displayable%
          [val-f val-f]
          [parent parent])]

    
    [(struct canvas-elt (scene-f callback))
     TODO]
    
    [(struct button-elt (val-f callback enabled?-f))
     TODO]
    
    [(struct drop-down-elt (val-f choices-f callback enabled?-f))
     TODO]
    
    [(struct text-field-elt (val-f callback enabled?-f))
     TODO]
    
    [(struct slider-elt (val-f min-f max-f callback enabled?-f))
     TODO]
  
    [(struct checkbox-elt (label-f val-f callback enabled?-f))
     TODO]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (embed-mixin super%)
  (class super%
    (inherit get-parent)
    (super-new)
    
    (define/public (get-top)
      (let ([p (get-parent)])
        (cond
          [(eq? p this)
           p]
          [else
           (send (get-parent) get-top)])))))




(define embed:top%
  (class* aligned-pasteboard% (embed<%>)
    (init init-world)
    
    (define *world* init-world)
    
    (define/public (get-top)
      this)
    
    (define/public (get-world)
      *world*)
    
    (define/public (set-world a-world)
      (set! *world* a-world))
    
    (super-new)))



(define embed:row%
  (class* (embed-mixin horizontal-alignment%) (embed<%>)
    (super-new)))



(define embed:col%
  (class* (embed-mixin vertical-alignment%) (embed<%>)
    (super-new)))



(define embed:displayable%
  (class* (embed-mixin snip-wrapper%) (embed<%>)
    (inherit get-top)
    (init-field val-f)
    
    (define inner-string-snip%
      (class string-snip%
        (init label)
        (super-make-object label)))
    
    (super-new
     [snip (new inner-string-snip% 
                [label (displayable->string 
                        (val-f (send (get-top) get-world)))])])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; debugging help: let's see a gui as a snip.
(define (show initial-world a-gui)
  (let* ([f (new frame% [label ""])]
         [e (new embed:top% [init-world initial-world])]
         [c (new editor-canvas%
                 [parent f]
                 [editor e])])
    (send f show #t)
    (gui->snip a-gui e)))



(define (test-show)
  (show 0 (col (message "hello world")
               (message "goodbye world"))))
