#lang scheme
(require (prefix-in plot: plot)
         scheme/sandbox
         "../../gui-world.ss")

(provide (all-defined-out))


(define WIDTH 400)
(define HEIGHT 400)


;; The world consists of the name of a function and the body
(define-struct world (name args body plot dirty?) #:transparent)
(define-updaters world)

(define initial-world 
  (make-world "f" (list "x") 
              "x"
              (empty-scene WIDTH HEIGHT)
              #t))



;; on-replot-button-pressed: world -> world
(define (on-replot-button-pressed a-world)
  (let* ([a-plot (plot:plot (plot:line 
                             (world-function a-world))
                            #:width WIDTH
                            #:height HEIGHT)])
    (update-world-dirty? 
     (update-world-plot a-world 
                        (place-image (put-pinhole a-plot 0 0)
                                     0
                                     0
                                     (empty-scene (image-width a-plot)
                                                  (image-height a-plot))))
     #f)))


;; on-text-field-change: world string -> world
(define (on-text-field-change a-world new-text)
  (update-world-dirty? 
   (update-world-body a-world new-text)
   #t))


;; on-canvas-redraw: world -> scene
(define (on-canvas-redraw a-world)
  (cond
    [(world-dirty? a-world)
     (let* ([out-of-sync-text (text "Out of sync" 10 "black")])
       (place-image out-of-sync-text
                    0 0
                    (place-image (nw:rectangle (image-width out-of-sync-text)
                                               (image-height out-of-sync-text)
                                               "solid"
                                               "yellow")
                                 0
                                 0
                                 (world-plot a-world))))]
    [else
     (world-plot a-world)]))


(define (define-function-message a-world)
  (format "(define (~a ~a) "
          (world-name a-world)
          (string-join (world-args a-world)
                       " ")))



;; world-function-sexpression: world -> s-expression
(define (world-function-as-sexpression a-world)
  `(define (,(string->symbol (world-name a-world))
            ,@(map string->symbol (world-args a-world)))
     ,(read (open-input-string (world-body a-world)))))


;; world-function: world -> (X Y ... -> number)
(define (world-function a-world)
  (let ([my-eval
         (make-evaluator 
          'lang/htdp-beginner
          (world-function-as-sexpression a-world))])
    
    (lambda args
      (my-eval `(,(string->symbol (world-name a-world))
                 ,@args)))))



(define view
  (col
   (canvas on-canvas-redraw)
   (button "Replot" on-replot-button-pressed)
   
   (message define-function-message)
   (text-field world-body on-text-field-change)
   (message ")")))


#;(big-bang initial-world view)