;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname adder) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "../gui-world.ss")

;; Simple counting example.  We show a text field and a button.  When the button is
;; pressed, we increment the number.

;; A world is a number.
(define initial-world 0)

;; increment-world-counter: world -> world
;; Increments the world when the button is pressed.
(define (increment-world-counter a-world)
  (+ a-world 1))

;; world-number-string: world -> string
(define (world-number-string a-world)
  (number->string a-world))


;; The gui consists of a message of the current contents
;; of the world, and a button for incrementing those contents.
(define a-gui
  (col
   (message world-number-string)
   (button "Press me!" increment-world-counter)))

(define last-world
  (gui-big-bang initial-world a-gui))
