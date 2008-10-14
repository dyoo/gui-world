#lang scheme/base
(require "../../gui-world.ss")


(define-struct config
  (extra        ;; extra
   pedal        ;; pedal
   ;; Add more when we figure out what to do here...
   ))
(define-updaters config) 


;; Extra
(define-struct extra (carrier?       ;; boolean
                      mudguard?      ;; boolean
                      lock?          ;; boolean
                      pump?          ;; boolean
                      bottle?        ;; boolean
                      basket?        ;; boolean 
                      cateye?        ;; boolean
                      sidereflex?    ;; boolean
                      frontreflex?   ;; boolean
                      propstand?))   ;; boolean
(define-updaters extra)

(define initial-extra (make-extra #f #f #f #f #f #f #f #f #f #f))


  
;; carrier-needs-mudguard-rule: extra -> boolean
(define (carrier-needs-mudguard-rule? extra)
  (implies (extra-carrier? extra)
           (extra-mudguard? extra)))


;; pump-bottle-exclusive-rule: extra -> boolean
(define (pump-bottle-exclusive-rule? extra)
  (not (and (extra-pump? extra)
            (extra-bottle? extra))))


;; Pedals
(define PEDALS (list  "Black Plastic"
                      "Campagnolo Chorus"
                      "Campagnolo Record"
                      "PD 5500"
                      "PD 6600"
                      "PD C101"
                      "PD C105"
                      "PD M434"
                      "PD M545" ))

(define PEDAL-TYPES (list "Standard"
                          "SPD"
                          "Clip"))

(define-struct pedal (sku               ;; (in PEDALS)
                      pedaltype))       ;; (in PEDAL-TYPES)
(define-updaters pedal)

(define initial-pedal (make-pedal "Black Plastic" "Standard"))




;; legal-pedal-config?: pedal -> boolean
;; Returns true if the pedal configuration is legal.
(define (legal-pedal-config? a-pedal)
  (and (member (list (pedal-sku a-pedal) (pedal-pedaltype a-pedal))
               '(("PD 6600" "SPD")
                 ("PD 5500" "SPD")
                 ("PD M545" "Clip")
                 ("PD M434" "Clip")
                 ("Campagnolo Record" "SPD")
                 ("Campagnolo Chorus" "SPD")
                 ("PD C105" "Standard")
                 ("Black Plastic" "Standard")
                 ("PD C101" "Standard")))
       #t))


;; rule-conj: rule ... -> rule
;; Conjoins all of the rules.
(define (rule-conj . rules)
  (lambda (world)
    (andmap (lambda (a-rule)
              (a-rule world))
            rules)))


;; rule-disj: rule ... -> rule
;; Disjoins all of the rules.
(define (rule-disj . rules)
  (lambda (world)
    (andmap (lambda (a-rule)
              (a-rule world))
            rules)))



;; legal-configuration?: config -> boolean
;; Returns true if the global configuration given is a legal one.
(define legal-configuration?
  (rule-conj (project carrier-needs-mudguard-rule? config-extra)
             (project pump-bottle-exclusive-rule? config-extra)
             (project legal-pedal-config? config-pedal)))



;; implies: boolean boolean -> boolean
(define (implies x y)
  (if x y #t))


(define initial-world
  (make-config initial-extra
               initial-pedal))



;; checkbox/rule: (gvalueof boolean) (gcallbackof boolean) -> checkbox
;; Creates a checkbox that is enabled so long as a change to the checkbox still leads to
;; a valid configuration.
(define (checkbox/rule val-f callback)
  (local [(define (ok-to-change? a-world)
            (legal-configuration? (callback a-world (not (val-f a-world)))))]
    (checkbox val-f callback ok-to-change?)))


;; drop-down/rule: (gvalueof string) (gvalueof (listof string)) (gcallbackof string) -> drop-down
;; Creates a dropdown whose choices are limited to the ones that lead to a valid configuration.
(define (drop-down/rule val-f choices-f callback)
  (local [(define (good-choices a-world)
            (filter (lambda (a-choice)
                      (legal-configuration? (callback a-world a-choice)))
                    (choices-f a-world)))]
    (drop-down val-f good-choices callback)))


;; The GUI.
(define extra-gui
  (local [(define (cb getter updater)
            (checkbox/rule (project getter config-extra)
                           (project/inject updater config-extra update-config-extra)))]
    (box-group "Extra Accessories"
               (col
                (row "Carrier?" (cb extra-carrier? update-extra-carrier?)) 
                (row "Mudguard?" (cb extra-mudguard? update-extra-mudguard?))
                (row "Lock?" (cb extra-lock? update-extra-lock?))
                (row "Pump?" (cb extra-pump? update-extra-pump?))
                (row "Bottle?" (cb extra-bottle? update-extra-bottle?))
                (row "Basket?" (cb extra-basket? update-extra-basket?))
                (row "Cateye?" (cb extra-cateye? update-extra-cateye?))
                (row "Side reflex?" (cb extra-sidereflex? update-extra-sidereflex?))
                (row "Front reflex?" (cb extra-frontreflex? update-extra-frontreflex?))
                (row "Propstand?" (cb extra-propstand? update-extra-propstand?))))))
  

(define pedal-gui
  (local [(define (dd getter choices updater)
            (drop-down/rule (project getter config-pedal)
                            (lambda (a-world) choices)
                            (project/inject updater config-pedal update-config-pedal)))]
    (box-group "Pedal"
               (col (row "SKU" (dd pedal-sku PEDALS update-pedal-sku))
                    (row "Pedal type" (dd pedal-pedaltype PEDAL-TYPES update-pedal-pedaltype))))))


(define main-gui
  (col (row pedal-gui extra-gui)))


(big-bang initial-world main-gui)