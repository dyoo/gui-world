#lang scheme
;; Pressing the button creates a new block element in the world.
(require "../../private/gui-world.ss"
         lang/posn)


;; A world is a list of blocks.
(define INITIAL-WORLD (list))

(define-struct block (elt    ;; a row
                      posn   ;; posn
                      ))


(define a-button 
  (button "Press me"
          (lambda (w)
            (cons (new-block w) w))))


;; new-block: world -> block
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
                                            100))])
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