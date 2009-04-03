;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname dynamic-button-text) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; A small example where a text field controls the label content
;; for all the other widgets.  This exercises the auto-resizing code.
(require "../gui-world.ss")

(define (on-button-press label)
  label)

(define (label-text label)
  label)

(define (update-label-text label new-text)
  new-text)

(define (ignore-bool-update old-val new-bool)
  old-val)

(define a-gui 
  (col 
   (text-field label-text update-label-text)
   (message label-text)
   (button label-text on-button-press)
   (box-group label-text (col))
   (checkbox label-text true ignore-bool-update)))

(gui-big-bang "hello world" a-gui)