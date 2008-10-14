#lang scheme/base
(require "../../gui-world.ss")






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
              (row "Propstand?" (checkbox extra-propstand? update-extra-propstand?)))))
              
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
  (box-group "Pedal"
             (col
              (row "SKU" 
                   (drop-down pedal-sku PEDALS update-pedal-sku))
              (row "Pedal type" 
                   (drop-down pedal-pedaltype PEDAL-TYPES update-pedal-pedaltype)))))



;; pedal-rule: pedal -> boolean
(define (pedal-rule a-pedal)
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






;; Stuff to do

;(define-struct person (gender      ;; (in GENDERS)
;                       height      ;; (in PERSON-HEIGHTS)
;                       biketype))  ;; (in BIKE-TYPES) 
;(define-updaters person)
;
;(define GENDERS (list "Male" "Female"))
;(define PERSON-HEIGHTS (list "150-160 cm"
;                             "160-170 cm"
;                             "170-180 cm"
;                             "180-190 cm"
;                             "190-200 cm"))
;(define BIKE-TYPES (list "City Bike"
;                         "Grandma Bike"
;                         "Mountain Bike"
;                         "Racer Bike"))



;; implies: boolean boolean -> boolean
(define (implies x y)
  (if x y #t))







(define-struct config
  (extra        ;; extra
   pedal        ;; pedal
   ;person       ;; person
   ;frame        ;; frame
   ;tires        ;; tires
   ;rims         ;; rims
   ;gear         ;; gears
   ;shoes
   ))      ;; shoes
(define-updaters config) 

(define initial-world
  (make-config initial-extra
               initial-pedal))



(define main-gui
  (row
   (project/inject/gui pedal-gui config-pedal update-config-pedal)
   (project/inject/gui extra-gui config-extra update-config-extra)))

(big-bang initial-world main-gui)