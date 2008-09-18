#lang scheme/base

(require scheme/gui/base
         scheme/match
         scheme/class)
(require (for-syntax scheme/base
                     scheme/list
                     scheme/struct-info))

(define ... 'FIXME)




;                                                   
;                                                   
;          ;;;           ;             ;;;          
;     ;;;    ;           ;               ;          
;    ;   ;   ;           ;               ;          
;   ;        ;     ;;;   ;;;;    ;;;     ;     ;;;  
;   ;        ;    ;; ;;  ;; ;;  ;   ;    ;    ;   ; 
;   ;   ;;   ;    ;   ;  ;   ;      ;    ;    ;     
;   ;    ;   ;    ;   ;  ;   ;   ;;;;    ;     ;;;  
;   ;    ;   ;    ;   ;  ;   ;  ;   ;    ;        ; 
;    ;   ;   ;    ;; ;;  ;; ;;  ;  ;;    ;    ;   ; 
;     ;;;     ;;   ;;;   ;;;;    ;; ;     ;;   ;;;  
;                                                   
;                                                   
;                                           ;   ;;  

(define *world* #f)
(define *width* #f)
(define *height* #f)
(define *frame* #f)

(define *on-redraw-callback* #f)




;                       
;                       
;                       
;     ;;;  ;    ; ;;;;; 
;    ;   ; ;    ;   ;   
;   ;      ;    ;   ;   
;   ;      ;    ;   ;   
;   ;   ;; ;    ;   ;   
;   ;    ; ;    ;   ;   
;   ;    ; ;    ;   ;   
;    ;   ; ;    ;   ;   
;     ;;;   ;;;;  ;;;;; 
;                       
;                       
;                ; ;  ;;



(define-struct form (elts))


(define-struct elt ())
;; An element is one of the following:
(define-struct (string-elt elt) (s))
(define-struct (button-elt elt) (label callback enabled?))


;; big-bang: number number world -> void
;; Shows the frame, creates the initial world.
(define (big-bang width height initial-world)
  (set! *width* width)
  (set! *height* height)
  (set! *world* initial-world)
  (set-and-show-frame)
  (change-world! initial-world))


;; set-and-show-frame: -> void
(define (set-and-show-frame)
  (set! *frame* (new frame% 
                     [label ""] 
                     [width *width*]
                     [height *height*]))
  (send *frame* show #t))



;; make-form: element+ -> form
(define (-make-form first-elt . rest-elements)
  (make-form 
   (coerse-strings-to-string-elts (cons first-elt rest-elements))))



;; coerse-strings-to-string-elts: (listof (or/c elt string)) -> (listof elt)
;; Helper to turn strings into string-elts.
(define (coerse-strings-to-string-elts elts)
  (map (lambda (elt)
         (cond [(string? elt)
                (make-string-elt elt)]
               [else
                elt]))
       elts))



;; make-button: string (world -> world) [enabled? boolean] -> element
(define (-make-button label callback [enabled? #t])
  (make-button-elt label callback enabled?))


;; on-redraw: (world -> form) -> void
;; Initializes the redrawing callback.
(define (on-redraw callback)
  (set! *on-redraw-callback* callback)
  (refresh!))


;; change-world!: world -> void
(define (change-world! new-world)
  (set! *world* new-world)
  (refresh!))

(define (refresh!)
  (when *on-redraw-callback*
    (let ([new-form (*on-redraw-callback* *world*)])
      (render-form-to-frame new-form *frame*))))


;; render-form-to-frame: form frame -> void
;; Clears out the contents of the frame, and adds the form elements in.
(define (render-form-to-frame a-form a-frame)
  ;; Remove all children
  (send a-frame change-children (lambda (subareas) '()))
  ;; Add a new child
  (let ([a-panel (new vertical-panel% [parent a-frame])])
    (for ([an-elt (form-elts a-form)])
      (render-elt! an-elt a-panel))))


;; render-elt!: elt container% -> void
;; Adds an elt to the gui container. 
(define (render-elt! an-elt a-container)
  (match an-elt
    [(struct string-elt (s))
     (new message% [label s]
          [parent a-container])]

    [(struct button-elt (label callback enabled?))
     (new button% [label label]
          [parent a-container]
          [callback (lambda (b e)
                      (change-world! (callback *world*)))]
          [enabled enabled?])]))





;; make-row: element+ -> element
(define (make-row first-elt . rest-elt)
  ...)


(define (make-disabled-button label)
  ...)


(define (make-drop-down default-value choices callback)
  ...)


(define (make-text-field default-value callback)
  ...)


(define (maybe-make-error msg)
  ...)



;; notify-error: world string -> world
;; For some period of time, every make-form after a notify-error will
;; Show some notification at the top of the form.
(define (notify-error world msg)
  ...)















;                              
;                              
;            ;                 
;   ;    ;                     
;   ;;  ;;                     
;   ;;  ;; ;;;     ;;;    ;;;  
;   ; ;; ;   ;    ;   ;  ;;  ; 
;   ; ;; ;   ;    ;      ;     
;   ; ;; ;   ;     ;;;   ;     
;   ;    ;   ;        ;  ;     
;   ;    ;   ;    ;   ;  ;;  ; 
;   ;    ; ;;;;;   ;;;    ;;;  
;                              
;                              
;                        ; ;;  



;; random-choice: X+ -> X
;; Given a sequence of elements, chooses one of them randomly.
(define (random-choice first-elt . rest-elts)
  (list-ref (cons first-elt rest-elts)
            (random (add1 (length rest-elts)))))



;; Not really a part of GUI.
;; Convenient syntax for defining all the replacing-attribute functions,
;; given a structure id.
;; Usage:
;; If we have a
;;     (define-struct posn (x y z))
;; then
;;     (define-replacers posn)
;; will expand out to definitions for replace-posn-x, replace-posn-y, replace-posn-z.
;; Each replacer takes the struct val and an attribute value, and produces a new struct val.
(define-syntax (define-updaters stx)
  (syntax-case stx ()
    [(_ a-struct-type)
     (let* ([info (extract-struct-info (syntax-local-value #'a-struct-type))]
            [fields 
             (map (lambda (accessor)
                    (datum->syntax accessor
                                   (string->symbol
                                    (substring
                                     (symbol->string (syntax-e accessor))
                                     (add1 (string-length
                                            (symbol->string
                                             (syntax-e
                                              #'a-struct-type))))))))
                  (fourth info))])
       (with-syntax ([(accessor ...) fields]
                     [(update ...) (map (lambda (id)
                                          (datum->syntax 
                                           stx
                                           (string->symbol
                                            (string-append "update-"
                                                           (symbol->string (syntax-e #'a-struct-type))
                                                           "-"
                                                           (symbol->string (syntax-e id))))))
                                        fields)])
         (let ([result
                (syntax/loc stx
                  (begin
                    (define (update a-struct-val new-val)
                      (struct-copy a-struct-type a-struct-val
                                   (accessor new-val)))
                    ...))])
           result)))]))




(provide 
 
 big-bang
 on-redraw
 
 (rename-out [-make-form make-form]
             [-make-button make-button])

 
 make-row
 make-disabled-button
 make-drop-down
 make-text-field
 maybe-make-error
 notify-error
 random-choice
 define-updaters)