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

(define extra-gui
  (project/inject/gui
   (box-group "Extra Accessories"
              (col
               (row "Carrier?" (checkbox extra-carrier? update-extra-carrier?))
               (row "Mudguard?" (checkbox extra-mudguard? update-extra-mudguard?))
               (row "Lock?" (checkbox extra-lock? update-extra-lock?))
               (row "Pump?" (checkbox extra-pump? update-extra-pump?))
               (row "Bottle?" (checkbox extra-bottle? update-extra-bottle?))
               (row "Basket?" (checkbox extra-basket? update-extra-basket?))
               (row "Cateye?" (checkbox extra-cateye? update-extra-cateye?))
               (row "Side reflex?" (checkbox extra-sidereflex? update-extra-sidereflex?))
               (row "Front reflex?" (checkbox extra-frontreflex? update-extra-frontreflex?))
               (row "Propstand?" (checkbox extra-propstand? update-extra-propstand?))))
   config-extra
   update-config-extra))
  

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

(define pedal-gui
  (project/inject/gui 
   (box-group "Pedal"
              (col (row "SKU" (drop-down pedal-sku PEDALS update-pedal-sku))
                   (row "Pedal type" (drop-down pedal-pedaltype PEDAL-TYPES update-pedal-pedaltype))))
   config-pedal
   update-config-pedal))



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


;; legal-configuration-status: config -> string
(define (legal-configuration-status a-config)
  (cond
    [(legal-configuration? a-config)
     "Legal configuration."]
    [else
     "Illegal configuration."]))



;; implies: boolean boolean -> boolean
(define (implies x y)
  (if x y #t))


(define initial-world
  (make-config initial-extra
               initial-pedal))

(define main-gui
  (col (row pedal-gui extra-gui)
       (message legal-configuration-status)))


(big-bang initial-world main-gui)