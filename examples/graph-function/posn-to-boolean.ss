;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname posn-to-boolean) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; posn -> boolean graph

(require "../../gui-world.ss")


;; The world consists of all the points that return true.

(define-struct world (mode posns))
(define-updaters world)


(define mode:clear 'clear)
(define mode:fill 'fill)


(define initial-world (make-world mode:fill empty))


(define (draw-world a-world)
  (place-posns (world-posns a-world) (empty-scene 500 500)))

(define (place-posns posns a-scene)
  (cond
    [(empty? posns)
     a-scene]
    [else
     (place-posns (rest posns)
                  (place-posn (first posns) a-scene))]))

(define (place-posn a-posn a-scene)
  (place-image (text (posn->string a-posn) 10 "black")
               (posn-x a-posn)
               (posn-y a-posn)
               (place-image (circle 5 "solid" "red")
                            (posn-x a-posn)
                            (posn-y a-posn)
                            a-scene)))



;; posn->string: posn -> string
;; Converts a posn to a printable representation.
(define (posn->string a-posn)
  (string-append "(" (number->string (posn-x a-posn))
                 "," (number->string (posn-y a-posn))
                 ")"))


(define (on-click world x y)
  (toggle-world-posn world (make-posn (snap-to-grid x) (snap-to-grid y))))

(define (toggle-world-posn a-world a-posn)
  (cond [(symbol=? (world-mode a-world) mode:fill)
         
         (cond
           [(contains-posn? (world-posns a-world) a-posn)
            a-world]
           [else
            (update-world-posns a-world
                                (cons a-posn (world-posns a-world)))])]
        [(symbol=? (world-mode a-world) mode:clear)
         (update-world-posns a-world
                             (remove-posn (world-posns a-world) a-posn))]))


(define (contains-posn? posns a-posn)
  (cond
    [(empty? posns)
     false]
    [(equal? (first posns) a-posn)
     true]
    [else
     (contains-posn? (rest posns) a-posn)]))

(define (remove-posn posns a-posn)
  (cond
    [(empty? posns)
     empty]
    [(equal? (first posns) a-posn)
     (rest posns)]
    [else
     (cons (first posns)
           (remove-posn (rest posns) a-posn))]))


(define (snap-to-grid n)
  (* 50 (round (/ n 50))))



(define (on-mode-drop-down a-world a-choice)
  (cond
    [(string=? a-choice "fill")
     (update-world-mode a-world mode:fill)]
    [(string=? a-choice "clear")
     (update-world-mode a-world mode:clear)]))
                         

(define view
  (col (canvas/callback draw-world on-click)
       (drop-down world-mode (list mode:fill mode:clear) on-mode-drop-down)))



(big-bang initial-world view)
