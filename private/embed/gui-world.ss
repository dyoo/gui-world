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
                   refresh! ;; when the world changes, redraw.
                   ))



;;; gui->snip: gui embed<%> -> embed<%>
;;; Translates a gui into a snip that's attached to the given
;;; parent.
(define (gui->snip a-gui parent)
  (match a-gui
    [(struct row-elt (elts))
     (let ([row (new embed:row%
                     [top (get-field top parent)]
                     [gui a-gui]
                     [parent parent])])
       (for ([elt elts])
         (gui->snip elt row))
       row)]
    
    [(struct column-elt (elts))
     (let ([col (new embed:col%
                     [top (get-field top parent)]
                     [gui a-gui]
                     [parent parent])])
       (for ([elt elts])
         (gui->snip elt col))
       col)]

    [(struct box-group-elt (val-f a-subelt enabled?-f))
     TODO]

    [(struct displayable-elt (val-f))
     (new embed:displayable%
          [top (get-field top parent)]
          [gui a-gui]
          [parent parent])]

    
    [(struct canvas-elt (scene-f callback))
     TODO]
    
    [(struct button-elt (val-f callback enabled?-f))
     (new embed:button%
          [top (get-field top parent)]
          [gui a-gui]
          [parent parent])]
          
    [(struct drop-down-elt (val-f choices-f callback enabled?-f))
     TODO]
    
    [(struct text-field-elt (val-f callback enabled?-f))
     TODO]
    
    [(struct slider-elt (val-f min-f max-f callback enabled?-f))
     TODO]
  
    [(struct checkbox-elt (label-f val-f callback enabled?-f))
     TODO]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define embed:top%
  (class* aligned-pasteboard% (embed<%>)
    (init init-world)
    (init-field (top this))
    (define *world* init-world)
        

    (inherit get-pasteboard)
    
    (define/public (get-world)
      *world*)
    
    (define/public (set-world a-world)
      (set! *world* a-world))
    
    (define/public (refresh!)
      (let* ([pb (get-pasteboard)]
             [snip (send pb find-first-snip)])
        (printf "I see ~s~n" snip)))

    
    (super-new)))



(define embed:row%
  (class* horizontal-alignment% (embed<%>)
    (init-field top)
    (init-field gui)

    (define/public (refresh!)
      TODO)
    
    (super-new)))



(define embed:col%
  (class* vertical-alignment% (embed<%>)
    (init-field top)
    (init-field gui)

    (define/public (refresh!)
      TODO)
    
    (super-new)))



(define embed:displayable%
  (class* snip-wrapper% (embed<%>)
    (init-field top)
    (init-field gui)
    
    (define inner-string-snip%
      (class string-snip%
        (init label)
        (super-make-object label)))
    
    (define/public (refresh!)
      TODO)
    
    (super-new
     [snip (new inner-string-snip% 
                [label (displayable->string 
                        ((displayable-elt-val-f gui)
                         (send top get-world)))])])))


(define embed:button%
  (class* snip-wrapper% (embed<%>)
    (init-field top)
    (init-field gui)
    
    
    (define inner-button-snip%
      (class text-button-snip%
        
        (super-new [label 
                    (displayable->string
                     ((button-elt-val-f gui) (send top get-world)))]
                   [callback 
                    (lambda (snip event)
                      (send top set-world
                            ((button-elt-callback gui) (send top get-world))))])))
    
    (define/public (refresh!)
      TODO)
    
    (super-new [snip (new inner-button-snip%)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





;; debugging help: let's see a gui as a snip.
(define (show initial-world a-gui)
  (let* ([f (new frame% [label ""])]
         [e (new embed:top% [init-world initial-world])]
         [c (new editor-canvas%
                 [parent f]
                 [editor e])])
    (send f show #t)
    (gui->snip a-gui e)
    (send e refresh!)))



(define (test-show)
  (show 0 (col 
           (message "hello world")
           (row
            (button "A button" (lambda (world) (add1 world)))
            (message (lambda (world)
                       (string-append 
                        "The world contains: "
                        (number->string world))))))))
