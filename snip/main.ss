#lang scheme/base

(require scheme/class
         scheme/gui/base
         scheme/contract
         scheme/list
         scheme/match
         framework/framework
         embedded-gui
         "../gui-world.ss")


(provide/contract [make-snip (any/c elt? . -> . (is-a?/c editor-snip:decorated%))])


;; make-snip: -> decorated-editor-snip%.
(define (make-snip initial-world a-gui)
  (new gui-world-snip% 
       [initial-world initial-world]
       [gui a-gui]))


#;(define (editor-snip->syntax a-snip)
    (let* ([editor (send a-snip get-editor)]
           [ip (open-input-text-editor editor
                                       0
                                       'end
                                       (lambda (a-snip)
                                         ;; fixme
                                         (send a-snip read-special #f 1 1 1)))])
      (read-syntax #f ip)))


(define gui-world-snip%
  (class* editor-snip:decorated% (readable-snip<%>)
    (inherit get-editor set-snipclass)
    
    (init initial-world)
    (init-field gui)
    
    (define world initial-world)
    
    
    (define (initialize)
      (super-new)
      (set-snipclass (get-snip-class))
      (new embedded-text-button% 
           [label "Edit"]
           [callback (lambda (snip event)
                       (thread
                        (lambda ()
                          (let ([new-world
                                 (channel-get (big-bang world gui))])
                            (set! world new-world)))))]
           [parent (get-editor)]))

    
    
    (define/override (make-editor)
      (new aligned-pasteboard%))
    
    
    (define/override (make-snip)
      (new gui-world-snip% 
           [initial-world world]
           [gui gui]))
    
    
    (define/override (copy)
      (new gui-world-snip%
           [initial-world world]
           [gui gui]))
    
    
    ;; write: editor-stream-out% -> void
    ;; Writes out the state of the editor to stream-out.
    (define/override (write stream-out)
      (void)
      #;(send (get-operator-editor) write-to-file stream-out 0 'eof)
      #;(send stream-out put (length operand-snips))
      #;(for ([i (in-range (length operand-snips))])
        (send (get-operand-editor i) write-to-file stream-out 0 'eof)))
    
    
    
    ;; read-special: file number number number -> syntax
    (define/public (read-special file line col pos)
      (void)
      #;(with-handlers ([exn:fail? (lambda (exn)
                                   (printf "exn: ~s~n" exn)
                                   (make-special-comment (exn-message exn)))])
        (datum->syntax
         #f
         `(,(editor-snip->syntax operator-snip) ,@(map editor-snip->syntax operand-snips))
         (list file line col pos 1))))
    
        
    (initialize)))


(define classname (format "~s" '(lib "main.ss" ("gui-world" "snip" 1 0))))


;; get-snip-class: -> snipclass
;; Returns the snipclass.  We do this indirectly to work around the issue
;; from the multiple eventspaces floating around in DrScheme land.
(define (get-snip-class)
  (send (get-the-snip-class-list) find classname))