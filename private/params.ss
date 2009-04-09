#lang scheme/base
(require scheme/gui/base)

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

(define current-world (make-parameter #f))
(define current-gui-world-eventspace (make-parameter (make-eventspace)))
(define current-top-window (make-parameter #f))
(define current-top-panel (make-parameter #f))
(define current-world-listeners (make-parameter '()))
(define current-stopped? (make-parameter (box #t)))
(define current-stop-when (make-parameter #f))
(define current-last-world-ch (make-parameter (make-channel)))
(define current-on-key-event-callback (make-parameter #f))






;; queue-on-world-thread: (-> void) -> void
;; Adds something for the current-gui-world-eventspace's thread to evaluate.
(define (queue-on-world-thread f)
  (parameterize ([current-eventspace (current-gui-world-eventspace)])
    (queue-callback f)))

(define (queue-on-eventspace es f)
  (parameterize ([current-eventspace es])
    (queue-callback f)))



;; change-world!: (-> world) -> void
;; Changes the world.
(define (change-world/f! new-world-f)
  (queue-on-world-thread
   (lambda ()
     (with-handlers ([void (lambda (exn)
                             (set-box! (current-stopped?) #t)
                             (raise exn))])
       (current-world (new-world-f (current-world)))
       (for ([listener (current-world-listeners)])
         (listener (current-world)))
       (when (and (current-stop-when) ((current-stop-when) (current-world)))
         (set-box! (current-stopped?) #t)
         (thread (lambda () (channel-put (current-last-world-ch) (current-world)))))))))



(provide (all-defined-out))