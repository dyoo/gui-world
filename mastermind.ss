#lang scheme
(require htdp/world
         "gui.ss")

;; Mastermind

;; The world is a
;; (make-mm c1 c2 g1 g2)
;; where c1, c2 are elements in '("red" "blue" "green")
;; and g1, g2 are elements in '("red" "blue" "green" "blank")
(define-struct mm (c1 c2 g1 g2) #:transparent)

(define-replacers mm)

(define COLORS (list "red" "blue" "green"))
(define COLORS+BLANK (cons "blank" COLORS))


(define initial-world 
  (make-mm (random-choice COLORS)
           (random-choice COLORS)
           "blank"
           "blank"))


;; on-g1-change: world string -> world
;; When the user changes the value of g1, we recompute the world.
(define (on-g1-change a-world a-val) 
  (replace-mm-g1 a-world a-val))


;; on-g2-change: world string -> world
;; When the user changes the value of g2, we recompute the world.
(define (on-g2-change a-world a-val) 
  (replace-mm-g2 a-world a-val))


;; form-filled?: world -> boolean
;; Returns true if the world is all filled out
(define (form-filled? a-world)
  (and (not (string=? (mm-g1 a-world) "blank"))
       (not (string=? (mm-g2 a-world) "blank"))))


;; win?: world -> boolean
;; Returns true if the world is in winning state.
(define (win? a-world)
  (string=? (mm-c1 a-world) (mm-g1 a-world))
  (string=? (mm-c2 a-world) (mm-g2 a-world)))



;; world->form-scene: world -> scene
;; Either shows an editable form if the game is still being played, or
;; a read-only form if the game is over.
(define (world->form-scene a-world)
  (cond
    [(form-filled? a-world)
     (make-form (make-row (mm-c1 a-world) (mm-c2 a-world))
                (make-row (mm-g1 a-world) (mm-g2 a-world))
                (cond [(win? a-world)
                       "You win!"]
                      [else
                       "You didn't win."]))]
    [else
     (make-form
      (make-row "???" "???")
      (make-row (make-drop-down (mm-g1 a-world) COLORS+BLANK on-g1-change)
                (make-drop-down (mm-g2 a-world) COLORS+BLANK on-g2-change)))]))
