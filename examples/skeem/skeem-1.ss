#lang scheme
(require lang/posn)

;; First version: Button which when pressed will create new editor elements on the
;; canvas.  User can't drag anything yet.


(define BOX-WIDTH 100)
(define MARGIN 75)
(define DEFAULT-HEIGHT 200)


;; An editor is a:
(define-struct editor (gui  ;; gui
                       posn ;; posn
                       val  ;; string
                       ))

(define-struct world (editors))


;; add-new-editor: world -> world
;; Adds a new editor to the world.
(define (add-new-editor a-world)
  (letrec ([new-gui (row (list (text-field (lambda (world)
                                             (editor-val (find-editor/gui new-gui world)))
                                           
                                           (lambda (world new-val)
                                             (update-editor/gui new-gui world new-val))))
                         (css
                          (lambda (a-world a-css)
                            (update-css-posn new-gui 
                                             (editor-posn (find-editor/gui new-gui a-world))
                                             a-css))))])
    
    (make-world (cons (make-editor new-gui "")
                      (world-editors a-world)))))


;; update-css-posn: world gui posn css -> css
(define (update-css-posn a-gui a-posn a-css)
  (update-css a-gui 'top (posn-y a-posn)
              (update-css a-gui 'left (posn-x a-posn) a-css)))
              

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
                


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stubs

(define (big-bang world view . handlers)
  (void))

;; update-css: css gui symbol value -> css
(define (update-css a-css a-gui a-property-name a-property-val)
  a-css)

(define (css handler)
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

(big-bang INITIAL-WORLD world->gui)