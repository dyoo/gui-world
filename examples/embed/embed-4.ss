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
  (let* ([a-msg 
          (message 
           (lambda (w) 
             "Block"))]
         
         [a-column
          (col a-msg
               #:css (lambda (w me css)
                       (css-update-posn 
                        css me 
                        (block-posn (find-block/elt w me)))))]
         [a-block
          (make-block a-column (make-posn (* (length w) 100) 
                                          200))])
    a-block))
          

;; css-update-posn: css elt posn -> css
;; Updates the css top/left attributes of an element at a posn.
(define (css-update-posn a-css an-elt a-posn)
  (css-update (css-update a-css an-elt 'top
                          (posn-y a-posn))
              an-elt 'left
              (posn-x a-posn)))


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
          #:css (css-update-posn (make-css) 
                                 a-button 
                                 (make-posn 0 100)))