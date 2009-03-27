#lang scheme
(require scheme/gui/base
         scheme/match
         scheme/class
         scheme/list
         scheme/bool
         scheme/contract
         htdp/image
         web-server/servlet-env
         web-server/servlet
         (only-in lang/htdp-beginner image?)
         (only-in srfi/1 list-index)
         
         "gui-struct.ss"
         "world-support.ss")

(define (gui->render an-elt)
  (match an-elt
    [(struct row-elt (elts))
     (lambda (world embed/url)
       `(table
         ,@(map (lambda (render)
                  `(tr (td ,(render world embed/url))))
                (map gui->render elts))))]    
    [(struct column-elt (elts))
     (lambda (world embed/url)
       `(table
         (tr
          ,@(map (lambda (render)
                   `(td ,(render world embed/url)))
                 (map gui->render elts)))))]    
    [(struct box-group-elt (val-f a-subelt enabled?-f))
     ; XXX
     (error 'box-group "Don't know what a box group should be")]    
    [(struct displayable-elt (val-f))
     (lambda (world embed/url)
       (val-f world))]    
    [(struct canvas-elt (scene-f callback))
     ; XXX
     (error 'canvas "Can't implement a canvas yet")]    
    [(struct button-elt (val-f callback enabled?-f))
     (lambda (world embed/url)
       `(form ([action 
                ,(embed/url
                  (lambda (req)
                    (printf "Button click!~n")
                    (callback world)))]
               [method "post"])
              (input ([type "submit"]
                      ,@(if (enabled?-f world)
                            empty
                            `([disabled "disabled"]))
                      [value
                       ,(val-f world)]))))]
    [(struct drop-down-elt (val-f choices-f callback enabled?-f))
     (lambda (world embed/url)
       (define name (symbol->string (gensym 'drop-down)))
       (define selection (val-f world))
       `(form ([action
                ,(embed/url
                  (lambda (req)
                    (callback
                     ; XXX Better error checking
                     (bytes->string/utf-8
                      (binding:form-value
                       (bindings-assq (string->bytes/utf-8 name)
                                      (request-bindings/raw req)))))))]
               [method "post"])
              (select ([name ,name]
                       ,@(if (enabled?-f world)
                             empty
                             `([disabled "disabled"])))
                      ,@(map (lambda (choice)
                               `(option
                                 ,(if (string=? choice selection)
                                      `([selected "selected"])
                                      empty)
                                 ,choice))
                             (choices-f world)))))]    
    [(struct text-field-elt (val-f callback enabled?-f))
     (lambda (world embed/url)
       (define name (symbol->string (gensym 'text-field)))
       `(form ([action
                ,(embed/url
                  (lambda (req)
                    (printf "text-field!~n")
                    (callback
                     ; XXX Better error checking
                     (bytes->string/utf-8
                      (binding:form-value
                       (bindings-assq (string->bytes/utf-8 name)
                                      (request-bindings/raw req)))))))]
               [method "post"])
              (input ([name ,name]
                      [type "text"]
                      ,@(if (enabled?-f world)
                            empty
                            `([disabled "disabled"])))
                     ,(val-f world))))]    
    [(struct slider-elt (val-f min-f max-f callback enabled?-f))
     ; XXX
     (error 'slider "Can't make a slider look nice yet")]    
    [(struct checkbox-elt (label-f val-f callback enabled?-f))
     (lambda (world embed/url)
       (define name (symbol->string (gensym 'checkbox)))
       `(form ([action
                ,(embed/url
                  (lambda (req)
                    (printf "checkbox!~n")
                    (match 
                        (bindings-assq (string->bytes/utf-8 name)
                                       (request-bindings/raw req))
                      [#f
                       (callback #f)]
                      [_
                       (callback #t)])))]
               [method "post"])
              ; XXX How to force submit?
              (input ([name ,name]
                      [type "checkbox"]
                      ,@(if (val-f world)
                            `([checked "on"])
                            empty)
                      ,@(if (enabled?-f world)
                            empty
                            `([disabled "disabled"])))
                     ,(label-f world))))]))

;; big-bang: world gui -> void
;; Shows the frame, creates the initial world.
(define (big-bang initial-world a-gui)
  (define render (gui->render a-gui))
  (serve/servlet
   (lambda (req)
     (let loop ([world initial-world])
       (loop
        (send/suspend/dispatch
         (lambda (embed/url)
           `(html
             (body
              ,(render world 
                       embed/url))))))))))

;; on-tick: number (world -> world) -> void
(define (on-tick freq callback)
  (error 'on-tick "Not supported"))

;; on-key-event: (world key -> world) -> void
(define (on-key-event callback)
  (error 'on-key-event "Not supported"))

(provide big-bang 
         on-tick 
         on-key-event
         
         elt? 
         row 
         col
         message
         button
         button/enabled
         slider
         slider/enabled
         drop-down
         drop-down/enabled
         text-field
         text-field/enabled
         checkbox
         checkbox/enabled
         canvas
         canvas/callback
         box-group
         box-group/enabled
         project/inject/gui)


(provide ;; Other helpers
 define-updaters
 update
 with-getter/updater
 
 update-color-red update-color-green update-color-blue
 update-posn-x update-posn-y
 color-red-accessor color-green-accessor color-blue-accessor
 posn-x-accessor posn-y-accessor
 
 
 random-choice
 place-image
 empty-scene
 nw:rectangle
 (all-from-out htdp/image))