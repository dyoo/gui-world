(module tool scheme/base
  (require drscheme/tool
           scheme/class
           scheme/unit
           scheme/gui/base)
  
  (provide tool@)
  
  (define-unit tool@
    
    (import drscheme:tool^)
    (export drscheme:tool-exports^)
    
    (dynamic-require 'gui-world/snip)
    
    (define (phase1)
      (drscheme:get/extend:extend-unit-frame frame-mixin))
    
    (define (phase2)
      (void))
    
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
                                   (schanzer-box:make-schanzer-box))))]))
        
        
        (initialize)))))