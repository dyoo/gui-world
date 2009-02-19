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


;;; Translates a gui into a snip.
(define (gui->snip a-gui [parent (new embed:top%)])
  (match a-gui
    [(struct row-elt (elts))
     TODO]
    
    [(struct column-elt (elts))
     TODO]
    
    [(struct box-group-elt (val-f a-subelt enabled?-f))
     TODO]

    [(struct displayable-elt (val-f))
     TODO]
    
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

(define embed<%> (interface ()))

(define embed:top%
  (class* aligned-pasteboard% (embed<%>)
    (super-new)))

(define embed:row%
  (class* vertical-alignment% (embed<%>)
    (super-new)))

(define embed:col%
  (class* horizontal-alignment% (embed<%>)
    (super-new)))

(define embed:displayable%
  (class* snip-wrapper% (embed<%>)
    
    (init-field val-f)
    
    (define inner-string-snip%
      (class string-snip%
        (init label)
        (super-make-object label)))
    
    (super-new
     [snip (new inner-string-snip% 
                [label "hello world"])])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; debugging help: let's see a snip.
(define (show a-snip-f)
  (let* ([f (new frame% [label ""])]
         [e (new embed:top%)]
         [c (new editor-canvas%
                 [parent f]
                 [editor e])])
    (send f show #t)
    (a-snip-f e)
    #;(send e insert a-snip)))

(define (test-show)
  (show (lambda (p)
          (new embed:displayable% 
               [val-f (lambda (a-world) "hello world")]
               [parent p]))))
