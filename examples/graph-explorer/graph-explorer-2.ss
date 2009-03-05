#lang scheme
(require (prefix-in plot: plot)
         scheme/sandbox
         lang/posn
         "../../gui-world.ss")

;; does parameteric

(provide (all-defined-out))


(define WIDTH 400)
(define HEIGHT 400)
(define T-MAX 400)

;; The world consists of the name of a function and the body
(define-struct world (name
                      args
                      x-name 
                      x-args
                      x-body
                      y-name
                      y-args
                      y-body
                      plot
                      x-plot
                      y-plot
                      dirty?) #:transparent)
(define-updaters world)

(define initial-world 
  (make-world "f"
              (list "t")
              "x" (list "t")
              "(cos t)"
              "y" (list "t")
              "t"
              (empty-scene WIDTH HEIGHT)
              (empty-scene WIDTH HEIGHT)
              (empty-scene WIDTH HEIGHT)
              #t))


;; 2d-plot->scene: 2d-plot -> scene
(define (2d-plot->scene a-plot)
  (place-image (put-pinhole a-plot 0 0)
               0
               0
               (empty-scene (image-width a-plot)
                            (image-height a-plot))))
  

;; world-replot: world -> world
(define (world-replot a-world)
  (with-handlers ([void
                   (lambda (exn)
                     (raise exn))])
    (let* ([wf (world-function a-world)]
           [f (lambda (t)
                (let ([a-posn (wf t)])
                  (vector (posn-x a-posn) (posn-y a-posn))))]
           [a-plot (2d-plot->scene
                    (plot:plot (plot:line f 
                                         #:mode 'parametric
                                         #:t-min 0
                                         #:t-max T-MAX)
                              #:width WIDTH
                              #:height HEIGHT
                              #:x-min 0
                              #:x-max WIDTH
                              #:y-min 0
                              #:y-max HEIGHT))]
           [an-x-plot (2d-plot->scene
                       (plot:plot (plot:line (lambda (t) (posn-x (wf t)))
                                            #:t-min 0
                                            #:t-max T-MAX)
                                 #:width WIDTH
                                 #:height HEIGHT
                                 #:x-label "t"
                                 #:x-min 0
                                 #:x-max WIDTH
                                 #:y-label "x"
                                 #:y-min 0
                                 #:y-max HEIGHT))]
           [a-y-plot (2d-plot->scene
                      (plot:plot (plot:line (lambda (t) (posn-y (wf t))) 
                                           #:t-min 0
                                           #:t-max T-MAX)
                                #:width WIDTH
                                #:height HEIGHT
                                #:x-label "t"
                                #:x-min 0
                                #:x-max WIDTH
                                #:y-label "y"
                                #:y-min 0
                                #:y-max HEIGHT))])
      (update-world-dirty? 
       (update-world-y-plot
        (update-world-x-plot
         (update-world-plot a-world a-plot)
         an-x-plot)
        a-y-plot)
       #f))))





;; world-function-sexpression: world -> s-expression
(define (world-function-as-sexpression a-world)
  `(define ,(string->symbol (world-name a-world))
     ,(world-function-as-lambda a-world)))



;; world-function-as-lambda: world -> s-expression
(define (world-function-as-lambda a-world)
  `(lambda (,@(map string->symbol (world-args a-world)))
     (make-posn ,(read (open-input-string (world-x-body a-world)))
                ,(read (open-input-string (world-y-body a-world))))))



;; world-function: world -> (X Y ... -> number)
(define (world-function a-world)
  (parameterize 
      ([sandbox-namespace-specs
        (let ([specs (sandbox-namespace-specs)])
          `(,(car specs)
            ,@(cdr specs)
            lang/posn
            ,@(if gui? '(mrlib/cache-image-snip) '())))]) 
    
    (let ([my-eval
           (make-evaluator 
            'lang/htdp-beginner
            (world-function-as-sexpression a-world))])
      
      (lambda args
        (my-eval `(,(string->symbol (world-name a-world))
                   ,@args))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gui code.


;; on-replot-button-pressed: world -> world
(define (on-replot-button-pressed a-world)
  (world-replot a-world))


;; on-x-text-field-change: world string -> world
(define (on-x-text-field-change a-world new-text)
  (update-world-dirty? 
   (update-world-x-body a-world new-text)
   #t))

;; on-y-text-field-change: world string -> world
(define (on-y-text-field-change a-world new-text)
  (update-world-dirty? 
   (update-world-y-body a-world new-text)
   #t))


;; on-canvas-redraw: world -> scene
(define ((on-canvas-redraw world-plot-selector) a-world)
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
                                 (world-plot-selector a-world))))]
    [else
     (world-plot-selector a-world)]))


;; define-function-message: world -> string
(define (define-function-message a-world)
  (format "(define (~a ~a) "
          (world-x-name a-world)
          (string-join (world-x-args a-world)
                       " ")))



(define view
  (col
   
   (row (col (canvas (on-canvas-redraw world-x-plot))
             (row "(define (" 
                  (message world-x-name)
                  (message (lambda (a-world)
                             (string-join (world-x-args a-world) " ")))
                  ")")
             (row (text-field world-x-body on-x-text-field-change) ")"))
        
        (col (canvas (on-canvas-redraw world-y-plot))
             (row "(define (" 
                  (message world-y-name)
                  (message (lambda (a-world)
                             (string-join (world-y-args a-world) " ")))
                  ")")
             (row (text-field world-y-body on-y-text-field-change) ")")))

   
   (button "Replot" on-replot-button-pressed)))