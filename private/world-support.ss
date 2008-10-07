#lang scheme/base

(require (for-syntax scheme/base
                     scheme/list
                     scheme/struct-info))

(require scheme/class
         htdp/error
         htdp/image
         (only-in lang/htdp-beginner image?))



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





;; random-choice: (listof X) -> X
;; Given a list of elements, chooses one of them randomly.
(define (random-choice elts)
  (list-ref elts
            (random (length elts))))



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







;; Copy-and-paste from htdp/world

(define (place-image image x y scene)
  (check-image 'place-image image "first")
  (check-arg 'place-image (number? x) 'integer "second" x)
  (check-arg 'place-image (number? y) 'integer "third" y)
  (check-scene 'place-image scene "fourth")
  (let ([x (number->integer x)]
        [y (number->integer y)])
    (place-image0 image x y scene)))

;; Symbol Any String String *-> Void
(define (check-image tag i rank . other-message)
  (if (and (pair? other-message) (string? (car other-message)))
      (check-arg tag (image? i) (car other-message) rank i)
      (check-arg tag (image? i) "image" rank i)))

;; Symbol Any String -> Void
(define (check-scene tag i rank)
  (if (image? i)
      (unless (scene? i)
        (error tag "scene expected, given image whose pinhole is at (~s,~s) instead of (0,0)"
               (pinhole-x i) (pinhole-y i)))
      (check-arg tag #f "image" rank i)))

(define (place-image0 image x y scene)
  (define sw (image-width scene))
  (define sh (image-height scene))
  (define ns (overlay/xy scene x y image))
  (define nw (image-width ns))
  (define nh (image-height ns))
  (if (and (= sw nw) (= sh nh)) ns (shrink ns 0 0 (- sw 1) (- sh 1)))) 

;; Symbol Any String -> Void
(define (check-pos tag c rank)
  (check-arg tag (and (number? c) (> (number->integer c) 0))
             "positive integer" rank c))

;; scene?: any -> boolean
(define (scene? i)
  (and (image? i)
       (= 0 (pinhole-x i))
       (= 0 (pinhole-y i))))

;; Number -> Integer
(define (number->integer x)
  (inexact->exact (floor x)))

;; empty-scene: number number -> scene
(define (empty-scene width height)
  (check-pos 'empty-scene width "first")
  (check-pos 'empty-scene height "second")    
  (put-pinhole 
   (overlay (rectangle width height 'solid 'white)
            (rectangle width height 'outline 'black))
   0 0))


(define (nw:rectangle width height mode color)
  (check-pos 'rectangle width "first")
  (check-pos 'rectangle height "second")
  (check-mode 'rectangle mode "third")
  (check-color 'rectangle color "fourth")
  (put-pinhole (rectangle width height mode color) 0 0))


;; MouseEvent -> MouseEventType
(define (mouse-event->symbol e)
  (cond [(send e button-down?) 'button-down]
        [(send e button-up?)   'button-up]
        [(send e dragging?)    'drag]
        [(send e moving?)      'move]
        [(send e entering?)    'enter]
        [(send e leaving?)     'leave]
        [else ; (send e get-event-type)
         (error 'on-mouse-event
                (format 
                 "Unknown event type: ~a"
                 (send e get-event-type)))]))

;; Symbol (union Symbol String) Nat -> Void
(define (check-mode tag s rank)
  (check-arg tag (or (eq? s 'solid)
                     (eq? s 'outline)
                     (string=? "solid" s)
                     (string=? "outline" s)) "mode (solid or outline)" rank s))

;; Symbol Any String -> Void
(define (check-color tag a-color rank)
  (check-arg tag (or (symbol? a-color) (string? a-color) (color? a-color)) 
             "color symbol or string" rank a-color))

(provide place-image
         empty-scene
         random-choice
         define-updaters
         nw:rectangle)