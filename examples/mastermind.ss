#lang scheme
(require "gui-world.ss")

;; Mastermind

;; The world is a
;; (make-mm c1 c2 g1 g2)
;; where c1, c2 are elements in '("red" "blue" "green")
;; and g1, g2 are elements in '("red" "blue" "green" "blank")
(define-struct mm (c1 c2 g1 g2))

(define-updaters mm)

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
  (update-mm-g1 a-world a-val))


;; on-g2-change: world string -> world
;; When the user changes the value of g2, we recompute the world.
(define (on-g2-change a-world a-val) 
  (update-mm-g2 a-world a-val))


;; form-filled?: world -> boolean
;; Returns true if the world is all filled out
(define (form-filled? a-world)
  (and (not (string=? (mm-g1 a-world) "blank"))
       (not (string=? (mm-g2 a-world) "blank"))))


;; win?: world -> boolean
;; Returns true if the world is in winning state.
(define (win? a-world)
  (and (string=? (mm-c1 a-world) (mm-g1 a-world))
       (string=? (mm-c2 a-world) (mm-g2 a-world))))



;; mm-c1-display: world -> string
;; Produces the content of the first color.  If the game isn't over yet, obscures it with ???.
(define (mm-c1-display a-world)
  (cond
    [(form-filled? a-world)
     (mm-c1 a-world)]
    [else
     "???"]))


;; mm-c2-display: world -> string
;; Produces the content of the second color.  If the game isn't over yet, obscures it with ???.
(define (mm-c2-display a-world)
  (cond
    [(form-filled? a-world)
     (mm-c2 a-world)]
    [else
     "???"]))


;; world-status-message: world -> string
;; Produces the status message at the bottom of the GUI.
(define (world-status-message a-world)
  (cond
    [(form-filled? a-world)
     (cond [(win? a-world)
            "You win!"]
           [else
            "You didn't win."])]
    [else ""]))
  

(define a-gui
  (col
   (row (message mm-c1-display) (message mm-c2-display))
   (row (drop-down mm-g1 COLORS+BLANK on-g1-change)
        (drop-down mm-g2 COLORS+BLANK on-g2-change))
   (message world-status-message)))


(big-bang initial-world a-gui)