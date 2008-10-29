;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname simple-slider) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "../gui-world.ss")

;; The world consists only of a single number.


;; get-val: number -> number
(define (get-val a-val)
  a-val)

;; update-val: number number -> number
(define (update-val old-val new-val)
  new-val)

;; status-message: number -> string
(define (status-message a-val)
  (string-append "The world holds: "
                 (number->string a-val)))
  
(define a-gui
  (col (slider get-val 0 5 update-val)
       (message status-message)))

(big-bang 0 a-gui)