#lang scheme
(require lang/posn)

;; Second version: allow the editors to be dragged along by their dragger.


(define BOX-WIDTH 100)
(define MARGIN 75)
(define DEFAULT-HEIGHT 200)

(define DRAGGER-HEIGHT 20)

;; An editor is a:
(define-struct editor (gui ;; gui
                       posn ;; where the editor is located
                       val ;; string
                       ))

(define-struct world (editors))


;; add-new-editor: world -> world
;; Adds a new editor to the world.
(define (add-new-editor a-world)
  (letrec ([new-gui (row (list (space BOX-WIDTH
                                      DRAGGER-HEIGHT
                                      (on-mouse 
                                       (lambda (a-world me event x y)
                                         (cond 
                                           [(world? a-world)
                                            (cond [(mouse-down? event)
                                                   (start-drag-mode a-world 
                                                                    (find-editor/gui new-gui a-world)
                                                                    x 
                                                                    y)]
                                                  [else a-world])]
                                           
                                           [(dragging-world? a-world)
                                            (cond
                                              [(mouse-up? event)
                                               (update-dragging-world-dragging? a-world #f)]
                                              [else
                                               (update-dragging-world-posn a-world 
                                                                           (make-posn x y))])]))))
                               
                               (text-field (lambda (world me)
                                             (editor-val (find-editor/gui new-gui world)))
                                           
                                           (lambda (world me new-val)
                                             (update-editor/gui new-gui world new-val))))
                         (css (lambda (world me a-css)
                                a-css)))])
    
    (make-world (cons (make-editor new-gui "")
                      (make-posn 0 0)
                      (world-editors a-world)))))





;; find-editor/gui: gui world -> editor
(define (find-editor/gui a-gui a-world)
  (findf (lambda (an-editor)
           (eq? a-gui (editor-gui an-editor)))
         (world-editors a-world)))


;; update-editor/gui: gui world string -> world
(define (update-editor/gui a-gui a-world a-val)
  (make-world (let loop ([editors (world-editors a-world)])
                (cond
                  [(empty? editors)
                   empty]
                  [(eq? (editor-gui (first editors))
                        a-gui)
                   (cons (make-editor (editor-gui (first editors))
                                      a-val)
                         (rest editors))]
                  [else
                   (cons (first editors)
                         (loop (rest editors)))]))))


(define plus-button 
  (button "+" add-new-editor))


(define (world->gui a-world)
  ;; A pasteboard has a list of gui elements.  Their positions will be defined
  ;; by the CSS.
  (pasteboard (cons plus-button 
                    (map editor-gui (world-editors a-world)))))
                


;; world->css: world -> css
;;
;; Style all of the editors so they're separated from each other.
(define (world->css world css)
  (let loop ([css css]
             [editors (world-editors world)]
             [i 0])
    (cond
      [(empty? editors)
       css]
      [else
       (loop (update-css-posn (editor-gui (first editors))
                              (make-posn (+ (* i BOX-WIDTH) MARGIN)
                                         DEFAULT-HEIGHT)
                              css)
             (rest editors)
             (add1 i))])))



;; update-css-posn: gui posn css -> css
(define (update-css-posn a-gui a-posn a-css)
  (update-css a-gui 'top (posn-y a-posn) 
              (update-css a-gui 'left (posn-y a-posn) a-css)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct dragging-world (dragged others posn dragging?))


(define (dragging-world->view w)
  ;; A pasteboard has a list of gui elements.  Their positions will be defined
  ;; by the CSS.
  (pasteboard (cons plus-button
                    (cons (editor-gui (dragging-world-dragged w))
                          (map editor-gui (dragging-world-others w))))))

;; During dragging, our world consists of the editor being dragged and the rest
;; of the editors, as well as the current mouse position and if we're still
;; dragging.
(define (start-drag-mode a-world editor posn)
  (let* ([dragging-world (make-dragging-world editor
                                             (remove editor (world-editors a-world))
                                             posn
                                             #t)]
         [updated-dragging-world
          (gui-big-bang dragging-world
                        (dragging-world->view)
                        (css dragging-world->css)
                        (stop-when (lambda (w)
                                     (not (dragging-world-dragging? w)))))])

    ;; fixme
    a-world))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stubs

(define (gui-big-bang world view . handlers)
  (void))

(define (update-dragging-world-dragging? w v)
  w)

(define (update-dragging-world-posn w v)
  w)

(define (on-mouse handler)
  (void))

;; mouse-down?: mouse-event -> boolean
(define (mouse-down? event)
  #f)
;; mouse-up?: mouse-event -> boolean
(define (mouse-up? event)
  #f)

;; update-css: css gui symbol value -> css
(define (update-css a-css a-gui a-property-name a-property-val)
  a-css)

(define (css handler)
  (void))

;; space: width height . args -> elt
(define (space width height . args)
  (void))

(define (width x)
  (void))

(define (height x)
  (void))

(define (row args)
  (void))

(define (text-field label get update)
  (void))

(define (button label on-press)
  (void))

(define (pasteboard elts)
  (void))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define INITIAL-WORLD (make-world empty))

(gui-big-bang INITIAL-WORLD world->gui
              (css world->css))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
