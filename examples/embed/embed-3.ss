#lang scheme
;; Pressing the button creates a new block element in the world.
(require "../../private/gui-world.ss"
         lang/posn)


;; A world is a list of blocks.
(define INITIAL-WORLD (list))


;; A block currently consists of the abstract gui element and a position.
(define-struct block (elt    ;; a col
                      posn   ;; posn
                      ))


;; The button, when pressed, will add a new block to the world.
(define a-button 
  (button "Press me"
          (lambda (w)
            (cons (new-block w) w))))


;; new-block: world -> block
;; Creates a new block that should be added to the world.
(define (new-block w)
  ;; Fixme: revision 4 needs to avoid the circularity introduced by the use
  ;; of letrec.  We'll cut the recursive knot by having all handlers take in the
  ;; element that fired the event.
  (letrec ([a-msg 
            (message 
             (lambda (w) 
               "Block"))]
           
           [a-column
            (col a-msg
                 #:css (lambda (w css)
                         (css-update 
                          (css-update css a-column 'top 
                                      (posn-y (block-posn (find-block/elt w a-column))))
                          a-column 'left
                          (posn-x (block-posn (find-block/elt w a-column))))))]

           [a-block
            (make-block a-column (make-posn (* (length w) 100) 
                                            200))])
    a-block))
          


;; find-block/elt: world -> block
;; Looks for the block with the given elt.
(define (find-block/elt w elt)
  (findf (lambda (b)
           (eq? (block-elt b) elt))
         w))
  

(define VIEW
  (pasteboard (lambda (w) 
                (cons a-button 
                      (map block-elt w)))))


(big-bang INITIAL-WORLD VIEW
          #:css (css-update (css-update (make-css) a-button 'top 0)
                            a-button 'left 100))