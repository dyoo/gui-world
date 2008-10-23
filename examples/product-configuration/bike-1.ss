;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bike-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(require "../../gui-world.ss")



(define UNKNOWN "Unknown")

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


(define-struct person (gender
                       height
                       biketype))

(define GENDERS (list UNKNOWN
                      "Male"
                      "Female"))

(define PERSON-HEIGHTS (list UNKNOWN 
                             "150-160 cm"
                             "160-170 cm"
                             "170-180 cm"
                             "180-190 cm"
                             "190-200 cm"))

(define BIKE-TYPES (list UNKNOWN
                           "City Bike"
                           "Grandma Bike"
                           "Mountain Bike"
                           "Racer Bike"))

(define initial-person (make-person UNKNOWN UNKNOWN UNKNOWN))




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



(define-struct rim (sku))         ;; (in RIMS)
(define-updaters rim)

(define initial-rim (make-rim UNKNOWN))

(define RIMS (list UNKNOWN
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

;; rim-height: rim -> string
(define (rim-height a-rim)
  (cond
    [(string=? (rim-sku a-rim) "MA3")
     "70 cm"]
    [(string=? (rim-sku a-rim) "T519")
     "70 cm"]
    [(string=? (rim-sku a-rim) "CXP 33")
     "70 cm"]
    [(string=? (rim-sku a-rim) "Open Pro")
     "70 cm"]
    [(string=? (rim-sku a-rim) "X221")
     "65 cm"]
    [(string=? (rim-sku a-rim) "Cosmos")
     "65 cm"]
    [(string=? (rim-sku a-rim) "Cross")
     "50 cm"]
    [(string=? (rim-sku a-rim) "Campagnolo Mexico Aero")
     "65 cm"]
    [(string=? (rim-sku a-rim) "Campagnolo Proton")
     "65 cm"]
    [(string=? (rim-sku a-rim) "Campagnolo Moskva Aero")
     "65 cm"]
    [(string=? (rim-sku a-rim) "Campagnolo Atlanta Aero")
     "65 cm"]
    [(string=? (rim-sku a-rim) "Helium")
     "50 cm"]
    [else 
     UNKNOWN]))



;; rim-width: rim -> string
(define (rim-width a-rim)
  (cond
    [(string=? (rim-sku a-rim) "MA3")
     ".85 cm"]
    [(string=? (rim-sku a-rim) "T519")
     "1.00 cm"]
    [(string=? (rim-sku a-rim) "CXP 33")
     "1.25 cm"]
    [(string=? (rim-sku a-rim) "Open Pro")
     "1.00 cm"]
    [(string=? (rim-sku a-rim) "X221")
     "1.50 cm"]
    [(string=? (rim-sku a-rim) "Cosmos")
     "1.25 cm"]
    [(string=? (rim-sku a-rim) "Cross")
     "1.50 cm"]
    [(string=? (rim-sku a-rim) "Campagnolo Mexico Aero")
     "1.00 cm"]
    [(string=? (rim-sku a-rim) "Campagnolo Proton")
     "0.85 cm"]
    [(string=? (rim-sku a-rim) "Campagnolo Moskva Aero")
     "0.85 cm"]
    [(string=? (rim-sku a-rim) "Campagnolo Atlanta Aero")
     "1.00 cm"]
    [(string=? (rim-sku a-rim) "Helium")
     "1.25 cm"]
    [else 
     UNKNOWN]))




(define HEIGHTS (list UNKNOWN
                      "50 cm"
                      "65 cm"
                      "70 cm"))
(define WIDTHS (list UNKNOWN
                     "0.85 cm"
                     "1.00 cm"
                     "1.25 cm"
                     "1.50 cm"
                     "1.75 cm"))


;; Pedals
(define PEDALS (list UNKNOWN
                     "Black Plastic"
                     "Campagnolo Chorus"
                     "Campagnolo Record"
                     "PD 5500"
                     "PD 6600"
                     "PD C101"
                     "PD C105"
                     "PD M434"
                     "PD M545" ))

(define PEDAL-TYPES (list UNKNOWN
                          "Standard"
                          "SPD"
                          "Clip"))

(define-struct pedal (sku))               ;; (in PEDALS)
(define-updaters pedal)

(define initial-pedal (make-pedal UNKNOWN))



;; pedal-pedaltype: pedal -> string
;; Returns the pedaltype of a-pedal.
(define (pedal-pedaltype a-pedal)
  (cond
    [(string=? (pedal-sku a-pedal) "Black Plastic")
     "Standard"]
    [(string=? (pedal-sku a-pedal) "Campagnolo Chorus")
     "SPD"]
    [(string=? (pedal-sku a-pedal) "Campagnolo Record")
     "SPD"]
    [(string=? (pedal-sku a-pedal) "PD 5500")
     "SPD"]
    [(string=? (pedal-sku a-pedal) "PD 6600")
     "SPD"]
    [(string=? (pedal-sku a-pedal) "PD C101")
     "Standard"]
    [(string=? (pedal-sku a-pedal) "PD C105")
     "Standard"]
    [(string=? (pedal-sku a-pedal) "PD M434")
     "Clip"]
    [(string=? (pedal-sku a-pedal) "PD M545")
     "Clip"]
    [else
     UNKNOWN]))







(define-struct tire (sku))        ;; in TIRES
(define-updaters tire)

(define TIRES (list UNKNOWN
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

(define initial-tire (make-tire UNKNOWN))



(define-struct gear (sku        ;; (in GEARS)
                     gears      ;; (in GEAR-NUMBER)
                     biketype   ;; (in BIKE-TYPES)
                     internal)) ;; boolean

(define GEARS (list UNKNOWN
                     "Acera"
                     "Campagnolo Avanti Ergopower"
                     "Campagnolo Mirage Ergopower"
                     "Campagnolo Veloce"
                     "Dura Ace"
                     "No gears"
                     "Shimano 105 STI"
                     "Shimano Acer"
                     "Shimano Deore"
                     "Shimano Nexus"
                     "Shimano RSX STI"
                     "Sora"
                     "Tiagra"
                     "Torpedo"
                     "Ultegra"))

(define GEAR-NUMBER (list "1"
                          "3"
                          "4"
                          "5"
                          "7"
                          "16"
                          "18"
                          "21"
                          "24"
                          "27"))


(define initial-gear (make-gear UNKNOWN UNKNOWN UNKNOWN #t))


(define-struct frame (sku color size))
(define FRAMES (list UNKNOWN
                     "Butterfly Classic"
                     "Centurion Basic"
                     "Centurion Basic Free"
                     "Centurion Basic Free Meral"
                     "Centurion Basic Light"
                     "Centurion Basic Light Meral"
                     "Centurion Basic Meral"
                     "Centurion Boulevard"
                     "Centurion Challenger"
                     "Centurion Challenger Lady"
                     "Centurion Crazy Point"
                     "Centurion Crazy Point Lady"
                     "Centurion Dark Image"
                     "Centurion Discovery"
                     "Centurion Discovery Lady"
                     "Centurion Eternity"
                     "Centurion Eternity Lady"
                     "Centurion Far Out"
                     "Centurion Helium"
                     "Centurion Invincible"
                     "Centurion Nitrogen"
                     "Centurion Off Duty"
                     "Centurion Oxygen"
                     "Centurion Oxygen Meral"
                     "Centurion Ultimate"
                     "Colibri Street Bike Plus"
                     "Faggin 7005"
                     "Faggin 7020"
                     "Faggin Easton"
                     "Jupiter Cruiser"
                     "Jupiter Inside"
                     "Jupiter Millenium"
                     "Jupiter Straight"
                     "Kildemoes Logic 32 Derailleur"
                     "Kildemoes Primates"
                     "Schwinn Mesa"
                     "Schwinn Moab 3"))

(define FRAME-COLORS (list UNKNOWN
                           "Black"
                           "Black Purple"
                           "Blue"
                           "Brown"
                           "Creme"
                           "Green"
                           "Grey"
                           "Light Blue"
                           "Light Green"
                           "Purple"
                           "Red"
                           "Silver"
                           "White"
                           "Yellow"))                             
(define FRAME-SIZES (list UNKNOWN
                          "13\""
                          "14\""
                          "15\""
                          "16\""
                          "17\""
                          "18\""
                          "19\""
                          "20\""
                          "21\""
                          "22\""
                          "23\""
                          "24\""
                          "25\""
                          "28\""))

(define initial-frame (make-frame UNKNOWN UNKNOWN UNKNOWN))



(define-struct shoes (sku))
(define-updaters shoes)

(define SHOES (list UNKNOWN
                    "No shoes"
                    "SH R072"
                    "SH R090"
                    "SH R150"
                    "SH R212"))
(define initial-shoes (make-shoes UNKNOWN))




(define-struct config
  (person       ;; person
   extra        ;; extra
   pedal        ;; pedal
   rim          ;; rim
   tire         ;; tire
   gear         ;; gear
   frame        ;; frame
   shoes        ;; shoe

   ))
(define-updaters config) 


(define initial-config
  (make-config initial-person
               initial-extra
               initial-pedal
               initial-rim
               initial-tire
               initial-gear
               initial-frame
               initial-shoes))







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



;; extra-is-good?: extra -> boolean
;; Returns true if the extra follows the rules.
(define (extra-is-good? an-extra)
  (and (implies (extra-carrier? an-extra)
                (extra-mudguard? an-extra))
       (not (and (extra-pump? an-extra)
                 (extra-bottle? an-extra)))))

;; rule-extra?: config -> boolean
(define (rule-extra? a-config)
  (extra-is-good? (config-extra a-config)))



;; rule-shoes?: config -> boolean 
(define (rule-shoes? a-config)
  (or (and (string=? (shoes-sku (config-shoes a-config)) "SH R212")
           (string=? (pedal-pedaltype (config-pedal a-config)) "SPD"))
      (and (string=? (shoes-sku (config-shoes a-config)) "SH R150")
           (string=? (pedal-pedaltype (config-pedal a-config)) "Clip"))
      (and (string=? (shoes-sku (config-shoes a-config)) "SH R090")
           (string=? (pedal-pedaltype (config-pedal a-config)) "SPD"))
      (and (string=? (shoes-sku (config-shoes a-config)) "SH R072")
           (string=? (pedal-pedaltype (config-pedal a-config)) "Clip"))
      
      (string=? (shoes-sku (config-shoes a-config)) "No shoes")))



;; rule-tire?: config -> boolean
;; Note: simplification doesn't take profile into account.
(define (rule-tire? a-config)
  (or (and (string=? (tire-sku (config-tire a-config)) "Triathlon")
           (string=? (rim-height (config-rim a-config)) "20 cm")
           (string=? (rim-width (config-rim a-config)) "1.00 cm"))
      (and (string=? (tire-sku (config-tire a-config)) "Courier")
           (string=? (rim-height (config-rim a-config)) "70 cm")
           (or (string=? (rim-width (config-rim a-config)) "0.85 cm")
               (string=? (rim-width (config-rim a-config)) "1.25 cm")))
      (and (string=? (tire-sku (config-tire a-config)) "Tecno")
           (string=? (rim-height (config-rim a-config)) "70 cm")
           (or (string=? (rim-width (config-rim a-config)) "1.00 cm")
               (string=? (rim-width (config-rim a-config)) "1.25 cm")))
      (and (string=? (tire-sku (config-tire a-config)) "Roma")
           (string=? (rim-height (config-rim a-config)) "70 cm")
           (or (string=? (rim-width (config-rim a-config)) "0.85 cm")
               (string=? (rim-width (config-rim a-config)) "1.00 cm")))
      (and (string=? (tire-sku (config-tire a-config)) "Lizzard")
           (string=? (rim-height (config-rim a-config)) "70 cm")
           (or (string=? (rim-width (config-rim a-config)) "1.50 cm")
               (string=? (rim-width (config-rim a-config)) "1.75 cm")))
      (and (string=? (tire-sku (config-tire a-config)) "Atlanta")
           (string=? (rim-height (config-rim a-config)) "70 cm")
           (string=? (rim-width (config-rim a-config)) "1.00 cm"))
      (and (string=? (tire-sku (config-tire a-config)) "Track")
           (string=? (rim-height (config-rim a-config)) "70 cm")
           (string=? (rim-width (config-rim a-config)) "0.85 cm"))
      (and (string=? (tire-sku (config-tire a-config)) "Extreme")
           (string=? (rim-height (config-rim a-config)) "70 cm")
           (string=? (rim-width (config-rim a-config)) "1.00 cm"))
      (and (string=? (tire-sku (config-tire a-config)) "All Weather")
           (string=? (rim-height (config-rim a-config)) "70 cm")
           (string=? (rim-width (config-rim a-config)) "1.25 cm"))
      (and (string=? (tire-sku (config-tire a-config)) "Beaumont")
           (string=? (rim-height (config-rim a-config)) "65 cm")
           (or (string=? (rim-width (config-rim a-config)) "1.25 cm")
               (string=? (rim-width (config-rim a-config)) "1.50 cm")))
      (and (string=? (tire-sku (config-tire a-config)) "Panaracer Avventura")
           (string=? (rim-height (config-rim a-config)) "65 cm")
           (or (string=? (rim-width (config-rim a-config)) "1.25 cm")
               (string=? (rim-width (config-rim a-config)) "1.50 cm")))
      (and (string=? (tire-sku (config-tire a-config)) 
                     "Panaracer Category Pro")
           (or (string=? (rim-height (config-rim a-config)) "65 cm")
               (string=? (rim-height (config-rim a-config)) "70 cm"))
           (or (string=? (rim-width (config-rim a-config)) "1.00 cm")
               (string=? (rim-width (config-rim a-config)) "1.25 cm")))
      (and (string=? (tire-sku (config-tire a-config)) 
                     "Panaracer Everride")
           (or (string=? (rim-height (config-rim a-config)) "65 cm")
               (string=? (rim-height (config-rim a-config)) "70 cm"))
           (or (string=? (rim-width (config-rim a-config)) "1.00 cm")
               (string=? (rim-width (config-rim a-config)) "1.25 cm")))
      (and (string=? (tire-sku (config-tire a-config)) 
                     "Panaracer Tourguard")
           (or (string=? (rim-height (config-rim a-config)) "65 cm")
               (string=? (rim-height (config-rim a-config)) "70 cm"))
           (or (string=? (rim-width (config-rim a-config)) "1.00 cm")
               (string=? (rim-width (config-rim a-config)) "1.25 cm")))
      (and (string=? (tire-sku (config-tire a-config)) 
                     "Panaracer Stradius")
           (or (string=? (rim-height (config-rim a-config)) "65 cm")
               (string=? (rim-height (config-rim a-config)) "70 cm"))
           (or (string=? (rim-width (config-rim a-config)) "0.85 cm")
               (string=? (rim-width (config-rim a-config)) "1.25 cm")))
      (and (string=? (tire-sku (config-tire a-config)) "Kenda")
           (string=? (rim-height (config-rim a-config)) "50 cm")
           (or (string=? (rim-width (config-rim a-config)) "1.25 cm")
               (string=? (rim-width (config-rim a-config)) "1.50 cm")))))

      
      
;; legal-configuration?: config -> boolean
;; Returns true if the global configuration given is a legal one.
(define (legal-configuration? a-config)
  (and (rule-extra? a-config)
       (rule-shoes? a-config)
       (rule-tire? a-config)))


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


;; legal-configuration-status: config -> string
;; Given a config, produces a string that reports on
;; that configuration's legality.
(define (legal-configuration-status a-config)
  (cond
    [(legal-configuration? a-config)
     "Legal configuration."]
    [else
     (string-append "Illegal configuration. ["
                    (broken-rule-message "Extra." (rule-extra? a-config))
                    (broken-rule-message "Shoes." (rule-shoes? a-config))
                    (broken-rule-message "Tires." (rule-tire? a-config))
                    "]")]))
                    

;; broken-rule-message: string boolean -> string
(define (broken-rule-message category ok?)
  (cond
    [ok?
     ""]
    [else category]))


(define pedal-gui
  (project/inject/gui 
   (box-group "Pedal"
              (col (row "SKU" 
                        (drop-down pedal-sku PEDALS update-pedal-sku))
                   (row "Pedal type" 
                        (message pedal-pedaltype))))
   config-pedal
   update-config-pedal))


(define shoes-gui
  (project/inject/gui
   (box-group "Shoes"
              (col (row "SKU"
                        (drop-down shoes-sku SHOES update-shoes-sku))))
   config-shoes
   update-config-shoes))


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
                        (message rim-height))
                   (row "Width" 
                        (message rim-width)))) 
   config-rim
   update-config-rim))


(define tire-gui
  (project/inject/gui 
   (box-group "Tires"
              (col (row "SKU"
                        (drop-down tire-sku TIRES update-tire-sku))))
   config-tire
   update-config-tire))


(define main-gui
  (col (row (col pedal-gui shoes-gui
                 rim-gui tire-gui) 
            extra-gui)
       (message legal-configuration-status)))


(big-bang initial-config main-gui)