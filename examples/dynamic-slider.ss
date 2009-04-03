;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname dynamic-slider) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Small example of a dynamic slider whose range changes based on button press.
;; When the buttons are pressed, the slider must change its range.
;;
;;
;; MrEd sliders don't natively support this feature, so we have to hack it for now.

(require "../gui-world.ss")


(define-struct world (k n))
(define-updaters world)


;; update-n-to-5: world -> world
(define (update-n-to-5 a-world)
  (update (world-n a-world) 5))


;; update-n-to-10: world -> world
(define (update-n-to-10 a-world)
  (update (world-n a-world) 10))


(define a-gui (col 
               (slider world-k 1 world-n update-world-k)
               (row (button "bump to 5" update-n-to-5)
                    (button "bump to 10" update-n-to-10))))

(define last-world
  (gui-big-bang (make-world 1 5) a-gui))
