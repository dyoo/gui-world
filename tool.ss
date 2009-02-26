#lang scheme/base
(require drscheme/tool
         scheme/class
         scheme/unit
         scheme/gui/base
         scheme/runtime-path
         "private/snip.ss")

(provide tool@)


(define-runtime-path this-directory ".")

(define-unit tool@  
  (import drscheme:tool^)
  (export drscheme:tool-exports^)
    
  (define drscheme-namespace (current-namespace))

  
  (define (phase1)
    (drscheme:get/extend:extend-unit-frame frame-mixin)
    (drscheme:get/extend:extend-interactions-text interactions-text-mixin))
  
  (define (phase2)
    (register-gui-world-sniptype! 
     "posn -> posn"
     '(lib "graph-function-difference.ss" 
           "gui-world" "examples" "graph-function"))

    (register-gui-world-sniptype!
     "number -> posn"
     '(lib "graph-function-time.ss"
           "gui-world" "examples" "graph-function"))
    
    (register-gui-world-sniptype!
     "posn event -> posn"
     '(lib "graph-function-event.ss"
           "gui-world" "examples" "graph-function")))

      
  (define (interactions-text-mixin super%)
    (class super%
      (inherit run-in-evaluation-thread)
      (define/override (evaluate-from-port port complete-program cleanup)
        (run-in-evaluation-thread 
         (lambda ()
           (void)
           ;; NOT WORKING YET
           #;(namespace-attach-module drscheme-namespace 
                                      '(lib "gui-world.ss" "gui-world"))))
        (super evaluate-from-port port complete-program cleanup))
      
      (super-new)))
  
  
  
  (define (frame-mixin super%)
    (class super%
      (inherit get-insert-menu
               get-edit-target-object)
      
      (super-new)
      
      (define tool-frame 
        (new frame% [label "Function tables"]))

      (define radio-box
        (new radio-box%
             [label "What kind of function?"]
             [choices (get-registry-names)]
             [parent tool-frame]))
      (new button%
           [parent tool-frame]
           [label "Insert"]
           [callback (lambda (b e)
                       (let ([choice
                              (send radio-box get-item-plain-label
                                    (send radio-box get-selection))])
                         (when (get-edit-target-object)
                           (send (get-edit-target-object) insert
                                 (make-gui-world-snip
                                  choice)))))])
      (new menu-item%
           [parent (get-insert-menu)]
           [label "Insert Graph Function"]
           [callback (lambda (menu-item control-event)
                       (send tool-frame show #t))]))))
                       