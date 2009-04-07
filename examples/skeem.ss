#lang scheme/base
(require "../gui-world.ss")

;; Expression builder.

;; FIXME: need to figure out if this is expressible in
;; Beginner Student Level.


;; TODO: discuss accessor design with Shriram to see if it's the right
;; thing to do.


;; Differences from the prototype:
;; Boxes can't be dragged.


;; An op is a 
(define-struct op (f            ;; (number number -> number)
                   name         ;; string
                   preposition   ;; string
                   ))

(define add-op (make-op + "Add" "to"))
(define subtract-op (make-op (lambda (x y) (- y x)) "Subtract" "from"))
(define multiply-op (make-op * "Multiply" "by"))
(define divide-op (make-op / "Divide" "by"))


                
;; A box is either a value box or an op (operation) box.
(define-struct value-box (val     ;; string
                          ))



(define-struct op-box (val        ;; string
                       op         ;; op
                       below      ;; box 
                       ))

(define-updaters value-box)
(define-updaters op-box)


;; make-add-to-op: string box -> box
(define (make-add-to-op val below)
  (make-op-box val add-op below))

;; make-subtract-from-op: string box -> box
(define (make-subtract-from-op val below)
  (make-op-box val subtract-op below))


;; make-multiply-by-op: string box -> box
(define (make-multiply-by-box val below)
  (make-op-box val multiply-op below))


;; make-divide-by-op: string box -> box
(define (make-divide-by-box val below)
  (make-op-box val divide-op below))


;; box-value: box -> number
(define (box-value a-box)
  (cond
    [(value-box? a-box)
     (string->number (value-box-val a-box))]
    [(op-box? a-box)
     ((op-f (op-box-op a-box))
      (string->number (op-box-val a-box))
      (box-value (op-box-below)))]))



;; The world is a box and a flag for refreshing the gui.
(define-struct world (box      ;; box 
                      refresh? ;; boolean
                      ))
(define-updaters world)


;; The initial world consists of a single value box. 
(define INITIAL-WORLD 
  (make-world (make-value-box "3")
              #f))



;; world->gui: world -> gui
;; Given a world, produce a gui for manipulating that world.
(define (world->gui a-world)  
  (col
   (row "button1" "button2" "button3")
   (box->gui (world-box a-world) world-box-accessor)))



;; box->gui: box (accessorof world box) -> gui
(define (box->gui a-box accessor)
  (cond
    [(value-box? a-box)
     (value-box->gui a-box accessor)]
    [(op-box? a-box)
     (op-box->gui a-box accessor)]))



;; value-box->gui: box (accessorof world value-box) -> gui
(define (value-box->gui a-box box-accessor)
  (let ([value-accessor
         (chain-accessors box-accessor value-box-val-accessor)])
  (box-group "The value " 
                (col (text-field (lambda (w)
                                   (get/accessor value-accessor w))
                                 (lambda (w v)
                                   (update/accessor value-accessor w v)))
                     (button "Recompute" 
                             (lambda (w)
                               ;; FIXME: use internal call to gui-world
                               (printf "~s~n"
                                       (box-value 
                                        (get/accessor box-accessor w)))
                               w))))))


;; op-box->gui: box (accessorof world op-box) -> gui
(define (op-box->gui a-box box-accessor)
  (let ([value-accessor
         (chain-accessors box-accessor op-box-val-accessor)])
    (col 
     (box-group (op-name (op-box-op a-box))
                (col
                 (text-field (lambda (w) (get/accessor value-accessor w))
                             (lambda (w v) (update/accessor value-accessor w v)))
                 (op-preposition (op-box-op a-box))
                 (button "Recompute"
                         (lambda (w)
                           (printf "~s~n"
                                   (box-value
                                    (get/accessor box-accessor w)))
                           w))))
     (op-box->gui (op-box-below a-box)
                  (chain-accessors box-accessor op-box-below-accessor)))))




;; clear-refresh: world -> world
;; Remove the refresh flag.
(define (clear-refresh a-world)
  (update-world-refresh? a-world #f))



;; Repeatedly call big-bang on an evolving world.
(let loop ([world INITIAL-WORLD])
  (loop 
   (gui-big-bang (clear-refresh world) (world->gui world)
                 (stop-when world-refresh?))))
