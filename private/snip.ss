#lang scheme
(require framework
         scheme/gui/base
         embedded-gui
         "calm-evt.ss"
         "gui-world.ss"
         (prefix-in world: htdp/world))

(provide (all-defined-out))




;; make-gui-world-snip: -> decorated-editor-snip%.
(define (make-gui-world-snip snipname)
  (let ([a-reg-entry (registry-lookup snipname)])
    (match a-reg-entry
      [(struct registry-entry 
               (name initial-world 
                     a-gui 
                     world->syntax 
                     world->bytes
                     bytes->world
                     world->thumbnail))
       (new gui-world-snip% 
            [initial-world initial-world]
            [registry-entry a-reg-entry])]
      [else
       (error 'make-gui-world-snip 
              "Unknown gui world sniptype ~s.  Found ~s"
              snipname a-reg-entry)])))


;; File format for gui-world snips:
;;
;;     * gui-world-sniptype as utf-8-encoded bytes, followed by the
;;     * bytes of the world representation.


(define gui-world-snip%
  (class* editor-snip:decorated% (readable-snip<%>)
    (inherit get-editor get-admin set-snipclass)
    
    (init initial-world)
    (init-field registry-entry)
    
    (define world initial-world)
    (define thumbnail-snip #f)
    (define edit-button #f)
    
    (define (initialize)
      (super-new)
      (set-snipclass (get-snip-class))
      (let ([horiz-container (new horizontal-alignment% [parent (get-editor)])])
        (set! thumbnail-snip (new image-snip%))
        (update-thumbnail-bitmap!)
        (new snip-wrapper%
             [parent horiz-container]
             [snip thumbnail-snip])
        (set! edit-button 
              (new embedded-text-button% 
                   [label "Edit"]
                   [callback (lambda (snip event)
                               (initiate-big-bang!))]
                   [parent horiz-container]))))
    
    ;; update-thumbnail-bitmap!: -> void
    (define (update-thumbnail-bitmap!)
      (let* ([world->bitmap (registry-entry-world->thumbnail registry-entry)]
             [new-image-snip (world->bitmap world)]
             [bm (send new-image-snip get-bitmap)])
        (send thumbnail-snip set-bitmap bm)))
    
    ;; update-the-world!: world -> void
    (define (update-the-world! new-world)
      (when (not (equal? world new-world))
        (set! world new-world)
        (update-thumbnail-bitmap!)
        (when (get-admin)
          (send (get-admin) modified this #t))))
    
    ;; Starts up the big bang.
    (define (initiate-big-bang!)
      (let* ([gui (registry-entry-gui registry-entry)]
             [ch (make-channel)]
             [calm-evt (make-calm-evt ch)])
        (thread 
         (lambda ()
           (big-bang world gui 
                     #:dialog? #t
                     #:on-world-change (lambda (new-world)
                                         (channel-put ch new-world))
                     #:on-close (lambda (new-world)
                                  (update-the-world! new-world)
                                  (channel-put ch new-world)))))
        (thread
         (lambda ()
           (let loop ()
             (update-the-world! (sync calm-evt))
             (loop))))))
    
    
    
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
      (send stream-out put (string->bytes/utf-8 (registry-entry-name 
                                                 registry-entry)))
      (send stream-out put ((registry-entry-world->bytes registry-entry)
                            world)))
    
    
    ;; read-special: file number number number -> syntax
    (define/public (read-special file line col pos)
      ((registry-entry-world->syntax registry-entry) world))
    
    
    (initialize)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snipclass stuff


(define classname 
  (format "~s" '(lib "tool.ss" ("gui-world" 1 0))))


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
    
    (define namespace (current-namespace))
    
    (define/public (set-namespace! a-namespace)
      (set! namespace a-namespace))
    
    (define/override (read in)
      (let* ([sniptype-name (bytes->string/utf-8 (send in get-bytes))]
             [world-bytes (send in get-bytes)])
        (let ([reg-entry 
               (parameterize ([current-namespace namespace])
                 (registry-lookup sniptype-name))])
          (match reg-entry
            [(struct registry-entry 
                     (name initial-world a-gui 
                           world->syntax world->bytes bytes->world
                           world->thumbnail))
             (new gui-world-snip% 
                  [initial-world (bytes->world world-bytes)]
                  [registry-entry reg-entry])]
            [else
             (error 'make-gui-world-snip 
                    "Unknown gui world sniptype ~s.  Found ~s" 
                    sniptype-name reg-entry)]))))
    
    (super-new)))


(let ([snipclass (make-object gui-world-snip-class%)])
  (send snipclass set-classname classname)
  (send snipclass set-version 1)
  (send (get-the-snip-class-list) add snipclass))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; A global registry entry table.
(define-struct registry-entry (name initial-world gui 
                                    world->syntax
                                    world->bytes
                                    bytes->world
                                    world->thumbnail))
(define *registry* (make-hash))


;; get-registry-names: -> (listof string)
;; Returns the names of the registered gui-world snips.
(define (get-registry-names)
  (for/list ([n (in-hash-keys *registry*)])
    n))


;; register-gui-world-sniptype!: string any elt -> void
;; Registers a new gui-world sniptype.
(define (register-gui-world-sniptype! name module-path)
  (when (not (hash-ref *registry* name #f))
    (hash-set! *registry* name module-path)))


;; registry-lookup: string -> (or/c false registry-entry)
;; Looks up the registry entries in the current namespace.
(define (registry-lookup name)
  (let ([path (hash-ref *registry* name #f)])
    (cond
      [(not path)
       #f]
      [else
       (let ([initial-world 
              (dynamic-require path 'initial-world)]
             [view
              (dynamic-require path 'view)]
             [world->syntax
              (dynamic-require path 'world->syntax)]
             [world->bytes
              (dynamic-require path 'world->bytes
                               (lambda () 
                                 (printf "Using default world->bytes.~n")
                                 default-world->bytes))]
             [bytes->world 
              (dynamic-require path 'bytes->world
                               (lambda () 
                                 (printf "Using default bytes->world.~n")
                                 default-bytes->world))]
             [world->thumbnail
              (dynamic-require path 'world->thumbnail
                               (lambda ()
                                 (printf "Using default thumbnail.~n")
                                 default-thumbnail))])
         (make-registry-entry name
                              initial-world
                              view
                              world->syntax
                              world->bytes
                              bytes->world
                              world->thumbnail))])))



;; world->bytes: world -> bytes
(define (default-world->bytes a-world)
  (let ([op (open-output-bytes)])
    (write a-world op)
    (get-output-bytes op)))

;; bytes->world: bytes -> world
(define (default-bytes->world a-bytes)
  (let ([ip (open-input-bytes a-bytes)])
    (read ip)))


(define (default-thumbnail a-world)
  (world:empty-scene 100 100))