(module tool scheme/base
  (require drscheme/tool
           scheme/class
           scheme/unit
           scheme/gui/base
           "snip/main.ss"
           (prefix-in gf-difference: 
                      "examples/graph-function/graph-function-difference.ss"))
  
  (provide tool@)
  
  (define-unit tool@
    
    (import drscheme:tool^)
    (export drscheme:tool-exports^)
    
    (dynamic-require 'gui-world/snip/main #f)
    
    (define (phase1)
      (drscheme:get/extend:extend-unit-frame frame-mixin))
    
    (define (phase2)
      (register-gui-world-sniptype! "gf-difference"
                                    gf-difference:initial-world
                                    gf-difference:view))
    
    (define (frame-mixin super%)
      (class super%
        (inherit get-insert-menu get-edit-target-object)
        (define (initialize)
          (super-new)
          (new menu-item%
               [parent (get-insert-menu)]
               [label "Insert Graph Function"]
               [callback (lambda (menu-item control-event)
                           (when (get-edit-target-object)
                             (send (get-edit-target-object) insert
                                   (make-gui-world-snip "gf-difference"))))]))
        
        
        (initialize)))))