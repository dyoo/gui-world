#lang scheme/base
(require "../../gui-world.ss")



;                                                                 
;                                                                 
;                                                                 
;                                                                 
;    ;;;;;     ;                                   ;              
;   ;;   ;     ;                                   ;              
;   ;        ;;;;;;     ; ;;;  ;;   ;;    ;;;;   ;;;;;;     ;;;;  
;   ;;         ;        ;;     ;;   ;;   ;;        ;       ;      
;    ;;;       ;        ;      ;;   ;;   ;         ;       ;      
;      ;;;     ;        ;      ;;   ;;  ;;         ;       ;;;    
;        ;;    ;        ;      ;;   ;;  ;;         ;         ;;;  
;        ;;    ;        ;      ;;   ;;   ;         ;           ;  
;   ;    ;;    ;;       ;       ;   ;;   ;;        ;;          ;  
;    ;;;;;      ;;;     ;       ;;; ;;    ;;;;      ;;;    ;;;;   
;                                                                 
;                                                                 
;                                                                 
;                                                         ;  ; ;;;




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




(define-struct rim (sku         ;; (in RIMS)
                    height      ;; (in HEIGHTS)
                    width))     ;; (in WIDTHS)
(define-updaters rim)

(define initial-rim (make-rim "Unknown" "Unknown" "Unknown"))

(define RIMS '("Unknown"
               "Campagnolo Atlanta Aero"
               "Campagnolo Mexico Aero"
               "Campagnolo Moskva Aero"
               "Campagnolo Proton"
               "Cosmos"
               "Cross"
               "CXP 33"
               "Helium"
               "MA3"
               "Open Pro"
               "T519"
               "X221")) 
(define HEIGHTS '("Unknown"
                  "50 cm"
                  "65 cm"
                  "70 cm"))
(define WIDTHS '("Unknown"
                     "0.85 cm"
                     "1.00 cm"
                     "1.25 cm"
                     "1.50 cm"
                     "1.75 cm"))



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


(define-struct tire (sku        ;; in TIRES
                     height     ;; in HEIGHTS
                     width      ;; in WIDTHS
                     profile))  ;; in TIRE-PROFILES
(define-updaters tire)

(define TIRES '("Unknown"
                "All Weather"
                "Atlanta"
                "Beaumont"
                "Courier"
                "Extreme"
                "Kenda"
                "Lizzard"
                "Panaracer Avventura"
                "Panaracer Category Pro"
                "Panaracer Everride"
                "Panaracer Stradius"
                "Panaracer Tourguard"
                "Roma"
                "Tecno"
                "Track"
                "Triatlon"))

(define TIRE-PROFILES '("Unknown"
                        "19 mm"
                        "20 mm"
                        "21 mm"
                        "22 mm"
                        "23 mm"
                        "25 mm"
                        "28 mm"
                        "30 mm"
                        "32 mm"
                        "35 mm"
                        "38 mm"))

(define initial-tire (make-tire "Unknown" 
                                "Unknown"
                                "Unknown"
                                "Unknown"))


(define-struct config
  (extra        ;; extra
   pedal        ;; pedal
   rim          ;; rim
   tire         ;; tire
   ;; Add more when we figure out what to do here...
   ))
(define-updaters config) 


(define initial-config
  (make-config initial-extra
               initial-pedal
               initial-rim
               initial-tire))






;                                               
;                                               
;                                               
;                     ;;;;                      
;   ;;;;;               ;;                      
;   ;;  ;;              ;;                      
;   ;    ;;  ;;   ;;    ;;       ;;;      ;;;;  
;   ;    ;   ;;   ;;    ;;      ;   ;    ;      
;   ;;;;;    ;;   ;;    ;;     ;     ;   ;      
;   ;;  ;;   ;;   ;;    ;;     ;;;;;;;   ;;;    
;   ;    ;   ;;   ;;    ;;     ;           ;;;  
;   ;    ;;  ;;   ;;    ;;     ;;            ;  
;   ;     ;   ;   ;;    ;;      ;    ;       ;  
;   ;     ;;  ;;; ;;     ;;;     ;;;;    ;;;;   
;                                               
;                                               
;                                               
;                                               



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
(define (legal-configuration? a-config)
  (and (carrier-needs-mudguard-rule? (config-extra a-config))
       (pump-bottle-exclusive-rule? (config-extra a-config))
       (legal-pedal-config? (config-pedal a-config))))


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






;                             
;                             
;                             
;                             
;     ;;;;   ;;   ;;  ;;;;;;  
;    ;;      ;;   ;;     ;    
;   ;;       ;;   ;;     ;    
;   ;        ;;   ;;     ;    
;   ;   ;;;  ;;   ;;     ;    
;   ;    ;;  ;;   ;;     ;    
;   ;     ;  ;;   ;;     ;    
;   ;;    ;  ;;   ;;     ;    
;    ;;  ;;  ;;   ;      ;    
;     ;;;;    ;;;;    ;;;;;;  
;                             
;                             
;                             
;                     ;;      

(define pedal-gui
  (project/inject/gui 
   (box-group "Pedal"
              (col (row "SKU" 
                        (drop-down pedal-sku PEDALS update-pedal-sku))
                   (row "Pedal type" 
                        (drop-down pedal-pedaltype 
                                   PEDAL-TYPES
                                   update-pedal-pedaltype))))
   config-pedal
   update-config-pedal))


(define extra-gui
  (project/inject/gui
   (box-group "Extra Accessories"
              (col
               (row "Carrier?" (checkbox extra-carrier? update-extra-carrier?))
               (row "Mudguard?" (checkbox extra-mudguard?
                                          update-extra-mudguard?))
               (row "Lock?" (checkbox extra-lock? update-extra-lock?))
               (row "Pump?" (checkbox extra-pump? update-extra-pump?))
               (row "Bottle?" (checkbox extra-bottle? update-extra-bottle?))
               (row "Basket?" (checkbox extra-basket? update-extra-basket?))
               (row "Cateye?" (checkbox extra-cateye? update-extra-cateye?))
               (row "Side reflex?"
                    (checkbox extra-sidereflex? update-extra-sidereflex?))
               (row "Front reflex?" 
                    (checkbox extra-frontreflex? update-extra-frontreflex?))
               (row "Propstand?" 
                    (checkbox extra-propstand? update-extra-propstand?))))
   config-extra
   update-config-extra))
  

(define rim-gui
  (project/inject/gui 
   (box-group "Rims"
              (col (row "SKU"
                        (drop-down rim-sku RIMS update-rim-sku))
                   (row "Height" 
                        (drop-down rim-height HEIGHTS update-rim-height))
                   (row "Width" 
                        (drop-down rim-width WIDTHS update-rim-width))))
   config-rim
   update-config-rim))


(define tire-gui
  (project/inject/gui 
   (box-group "Tires"
              (col (row "SKU"
                        (drop-down tire-sku TIRES update-tire-sku))
                   (row "Height" 
                        (drop-down tire-height HEIGHTS update-tire-height))
                   (row "Width" 
                        (drop-down tire-width WIDTHS update-tire-width))
                   (row "Profile" 
                        (drop-down tire-profile TIRE-PROFILES
                                   update-tire-profile))))
   config-tire
   update-config-tire))


(define main-gui
  (col (row pedal-gui extra-gui rim-gui)
       (row tire-gui)
       (message legal-configuration-status)))


(big-bang initial-config main-gui)