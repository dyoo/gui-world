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

(define image-directory
  (make-parameter
   (find-system-path 'temp-dir)))

; scene->file : scene path -> path
; saves the scene to file in the directory and returns the path
(define (scene->file s dir)
  (define filepath (make-temporary-file	"tmp~a.png" #f dir))
  (define width (image-width s))
  (define height (image-height s))
  (define bm (make-object bitmap% width height))
  (define dc (make-object bitmap-dc% bm))
  (send dc clear)
  (send s draw dc 0 0 0 0 width height 0 0 #f)
  (send bm save-file filepath 'png)
  filepath)

(define (gui->render an-elt)
  (match an-elt
    [(struct row-elt (elts))
     (define guis (map gui->render elts))
     (lambda (world embed/url)
       `(table
         (tr
          ,@(map (lambda (render)
                   `(td ,(render world embed/url)))
                 guis))))]    
    [(struct column-elt (elts))
     (define guis (map gui->render elts))
     (lambda (world embed/url)
       `(table
         ,@(map (lambda (render)
                  `(tr (td ,(render world embed/url))))
                guis)))]    
    [(struct box-group-elt (val-f a-subelt enabled?-f))
     (define subelt-render (gui->render a-subelt))
     ; XXX Handle enabled?-f
     (lambda (world embed/url)
       `(fieldset
         (legend ,(val-f world))
         ,(subelt-render world embed/url)))]    
    [(struct displayable-elt (val-f))
     (lambda (world embed/url)
       (displayable->string (val-f world)))]    
    [(struct canvas-elt (scene-f callback))
     (define name (symbol->string (gensym 'img)))
     (lambda (world embed/url)
       ; XXX Maybe we can be smart by hashing the scene so we generate fewer images
       (define the-scene (scene-f world))
       (define image-path (scene->file the-scene (image-directory)))
       (define-values (base file-name must-be-dir?) (split-path image-path))
       `(form ([action
                ,(embed/url
                  (lambda (req)
                    (define (get-pos f)
                      ; XXX Better error checking
                      (string->number
                       (bytes->string/utf-8
                        (binding:form-value
                         (bindings-assq (string->bytes/utf-8 f)
                                        (request-bindings/raw req))))))
                    (define x-pos (get-pos (format "~a.x" name)))
                    (define y-pos (get-pos (format "~a.y" name)))
                    (callback world x-pos y-pos)))])
              (input ([type "image"] [name ,name] [src ,(format "/~a" file-name)]))))]
    [(struct button-elt (val-f callback enabled?-f))
     (lambda (world embed/url)
       `(form ([action 
                ,(embed/url
                  (lambda (req)
                    (callback world)))]
               [method "post"])
              (input ([type "submit"]
                      ,@(if (enabled?-f world)
                            empty
                            `([disabled "disabled"]))
                      [value
                       ,(val-f world)]))))]
    [(struct drop-down-elt (val-f choices-f callback enabled?-f))
     (define name (symbol->string (gensym 'dropdown)))
     (define form-name (symbol->string (gensym 'dropdownform)))
     (lambda (world embed/url)       
       (define selection (val-f world))
       `(form ([name ,form-name]
               [action
                ,(embed/url
                  (lambda (req)
                    (callback
                     world
                     ; XXX Better error checking
                     (bytes->string/utf-8
                      (binding:form-value
                       (bindings-assq (string->bytes/utf-8 name)
                                      (request-bindings/raw req)))))))]
               [method "post"])
              (select ([name ,name]
                       [onchange ,(format "document.~a.submit()" form-name)]
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
     (define name (symbol->string (gensym 'textfield)))
     (lambda (world embed/url)       
       `(form ([action
                ,(embed/url
                  (lambda (req)
                    (printf "text-field!~n")
                    (callback
                     world
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
     (lambda (world embed/url)
       (define val (val-f world))
       (define min-v (min-f world))
       (define max-v (max-f world))
       (define len (number->string (string-length (number->string max-v))))
       (define down-name (symbol->string (gensym 'sliderdown)))
       (define up-name (symbol->string (gensym 'sliderup)))
       (define val-name (symbol->string (gensym 'sliderval)))
       `(form ([action
                ,(embed/url
                  (lambda (req)
                    (define new-val 
                      (match (bindings-assq (string->bytes/utf-8 down-name) (request-bindings/raw req))
                        [#f
                         (match (bindings-assq (string->bytes/utf-8 up-name) (request-bindings/raw req))
                           [#f
                            (string->number
                             (bytes->string/utf-8
                              (binding:form-value
                               (bindings-assq (string->bytes/utf-8 val-name) (request-bindings/raw req)))))]
                           [_
                            (min max-v (add1 val))])]
                        [_
                         (max min-v (sub1 val))]))
                    (callback world new-val)))])
              (table
               (tr
                (td 
                 (input ([name ,down-name]
                         [type "submit"]
                         [value "-"]
                         ,@(if (val . <= . min-v)
                               `([disabled "disabled"])
                               empty)
                         )))
                (td
                 (input ([name ,val-name]
                         [type "text"]
                         [size ,len]
                         [value ,(number->string val)]
                         [disabled "disabled"])))
                (td 
                 (input ([name ,up-name]
                         [type "submit"]
                         [value "+"]
                         ,@(if (max-v . <= . val)
                               `([disabled "disabled"])
                               empty))))))))]
    [(struct checkbox-elt (label-f val-f callback enabled?-f))
     (define name (symbol->string (gensym 'checkbox)))
     (define form-name (symbol->string (gensym 'checkboxform)))
     (lambda (world embed/url)       
       `(form ([name ,form-name]
               [action
                ,(embed/url
                  (lambda (req)
                    (match 
                        (bindings-assq (string->bytes/utf-8 name)
                                       (request-bindings/raw req))
                      [#f
                       (callback world #f)]
                      [_
                       (callback world #t)])))]
               [method "post"])
              (input ([name ,name]
                      [onchange ,(format "document.~a.submit()" form-name)]
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
                       embed/url))))))))
   #:extra-files-paths
   (list (image-directory))))

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