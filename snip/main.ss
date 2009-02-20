#lang scheme/base

(require scheme/class
         scheme/gui/base
         scheme/contract
         scheme/list
         scheme/match
         framework/framework
         embedded-gui
         "../gui-world.ss")




;; A global registry entry table.
(define-struct registry-entry (name initial-world gui world->syntax))
(define *registry* (make-hash))


;; register-gui-world-sniptype!: string any elt -> void
;; Registers a new gui-world sniptype.
(define (register-gui-world-sniptype! name 
                                      #:initial-world initial-world
                                      #:gui gui 
                                      #:world->syntax world->syntax
                                      #:world->bytes 
                                      (world->bytes
                                       default-world->bytes)
                                      #:bytes->world
                                      (bytes->world
                                       default-bytes->world))
  (hash-set! *registry* name 
             (make-registry-entry name
                                  initial-world
                                  gui 
                                  world->syntax)))

;; registry-lookup: string -> (or/c false registry-entry)
(define (registry-lookup name)
  (hash-ref *registry* name #f))




;; make-gui-world-snip: -> decorated-editor-snip%.
(define (make-gui-world-snip snipname)
  (match (registry-lookup snipname)
    [(struct registry-entry 
             (name initial-world a-gui world->syntax))
     (new gui-world-snip% 
          [initial-world initial-world]
          [registry-entry registry-entry])]
    [else
     (error 'make-gui-world-snip 
            "Unknown gui world sniptype ~s" snipname)]))



(define gui-world-snip%
  (class* editor-snip:decorated% (readable-snip<%>)
    (inherit get-editor set-snipclass)
    
    (init initial-world)
    (init-field registry-entry)
    
    (define world initial-world)
    
    
    (define (initialize)
      (super-new)
      (set-snipclass (get-snip-class))
      (new embedded-text-button% 
           [label "Edit"]
           [callback (lambda (snip event)
                       (initiate-big-bang!))]
           [parent (get-editor)]))

    
    ;; Starts up the big bang.
    (define (initiate-big-bang!)
      (thread
       (lambda ()
         (let* ([gui (registry-entry-gui registry-entry)]
                [new-world
                 (channel-get (big-bang world gui))])
           (set! world new-world)))))
    
    
    (define/override (make-editor)
      (new aligned-pasteboard%))
    
    
    (define/override (make-snip)
      (new gui-world-snip% 
           [initial-world world]
           [registry-entry registry-entry]))    
    
    
    (define/override (copy)
      (make-snip))
    
    
    ;; write: editor-stream-out% -> void
    ;; Writes out the state of the editor to stream-out.
    (define/override (write stream-out)
      ;; Format:
      ;; gui-world-sniptype as utf-8-encoded bytes, followed by the
      ;; bytes of the world representation.
      (send stream-out put (string->bytes/utf-8 ...)))
        
    
    ;; read-special: file number number number -> syntax
    (define/public (read-special file line col pos)
      (world->syntax world))
    
    
    (initialize)))


;; world->bytes: world -> bytes
(define (default-world->bytes a-world)
  (let ([op (open-output-bytes)])
    (write a-world op)
    (get-output-bytes op)))

;; bytes->world: bytes -> world
(define (default-bytes->world a-bytes)
  (let ([ip (open-input-bytes a-bytes)])
    (read ip)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snipclass stuff


(define classname 
  (format "~s" '(lib "main.ss" ("gui-world" "snip" 1 0))))


;; get-snip-class: -> snipclass
;; Returns the snipclass.  We do this indirectly to work around the issue
;; from the multiple eventspaces floating around in DrScheme land.
(define (get-snip-class)
  (send (get-the-snip-class-list) find classname))

(define gui-world-snip-class%
  (class snip-class%
    (inherit get-classname
             set-classname
             set-version)
    
    (define (initialize)
      (super-new))
    
    (define/override (read in)
      ...)))

(let ([snipclass (make-object gui-world-snip-class%)])
  (send snipclass set-classname classname)
  (send snipclass get-version 1)
  (send (get-the-snip-class-list) add snipclass))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Contracts

(provide/contract [make-gui-world-snip 
                   (string? . -> . (is-a?/c editor-snip:decorated%))]

                  [struct registry-entry 
                          ([name string?]
                           [initial-world any/c]
                           [gui elt?]
                           [world->syntax (any/c . -> . syntax?)])]
                  
                  [register-gui-world-sniptype! 
                   (string? #:initial-world any/c
                            #:gui elt? 
                            #:world->syntax (any/c . -> . syntax?) 
                            . -> . any)])