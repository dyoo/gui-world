;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname key) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "../gui-world.ss")


(define (show-world a-world)
  (place-image (text (format "~s" a-world) 10 "black")
               0
               0
               (empty-scene 100 100)))

(define view
  (canvas show-world))


(define (handle-key world key)
  (cond
    [(key=? key 'left)
     'left]
    [(key=? key 'right)
     'right]
    [(key=? key 'up)
     'up]
    [(key=? key 'down)
     'down]
    [(char? key)
     key]
    [else
     world]))


(big-bang "???" view)
(on-key-event handle-key)