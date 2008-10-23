;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bike-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
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




;; Extra
(define-struct extra (carrier?       ;; boolean
                      mudguard?      ;; boolean
                      lock?          ;; boolean
                      pump?          ;; boolean
                      bottle?        ;; boolean
                      basket?))        ;; boolean 
(define-updaters extra)

(define initial-extra (make-extra false false false false false false))



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
  (extra        ;; extra
   pedal        ;; pedal
   rim          ;; rim
   tire         ;; tire
   shoes        ;; shoe
   ))

(define-updaters config) 


(define initial-config
  (make-config initial-extra
               initial-pedal
               initial-rim
               initial-tire
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
;; Returns true if the extra accessories match the rules.
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


;; config-shoes-sku-choices: config -> (listof string)
;; Returns a list of the legal shoes we can use with the given configuration.
(define (config-shoes-sku-choices a-config)
  (filter-config-shoes-sku-candidates a-config SHOES))


;; legal-config-shoes-sku-candidate?: config string -> boolean
;; Produces true if the candidate sku leads to a legal configuration
;; in terms of shoes.
(define (legal-config-shoes-sku-candidate? a-config a-candidate)
  (cond
    [(string=? a-candidate UNKNOWN)
     true]
    [else
     (rule-shoes? (update (shoes-sku (config-shoes a-config)) 
                          a-candidate))]))
        

;; filter-config-shoes-sku-candidates: config (listof string) -> (listof string)
(define (filter-config-shoes-sku-candidates a-config candidates)
  (cond
    [(empty? candidates)
     empty]
    [else
     (cond       
       [(legal-config-shoes-sku-candidate? a-config (first candidates))
        (cons (first candidates) 
              (filter-config-shoes-sku-candidates a-config
                                                  (rest candidates)))]
       [else
        (filter-config-shoes-sku-candidates a-config 
                                            (rest candidates))])]))
             
              


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

      



;; config-tire-sku-choices: config -> (listof string)
;; Returns a list of the legal tires we can use with the given configuration.
(define (config-tire-sku-choices a-config)
  (filter-config-tire-sku-candidates a-config TIRES))


;; legal-config-tire-sku-candidate?: config string -> boolean
;; Produces true if the candidate sku leads to a legal configuration
;; in terms of shoes.
(define (legal-config-tire-sku-candidate? a-config a-candidate)
  (cond
    [(string=? a-candidate UNKNOWN)
     true]
    [else
     (rule-tire? (update (tire-sku (config-tire a-config)) 
                          a-candidate))]))


;; filter-config-tire-sku-candidates: config (listof string) -> (listof string)
(define (filter-config-tire-sku-candidates a-config candidates)
  (cond
    [(empty? candidates)
     empty]
    [else
     (cond       
       [(legal-config-tire-sku-candidate? a-config (first candidates))
        (cons (first candidates) 
              (filter-config-tire-sku-candidates a-config
                                                 (rest candidates)))]
       [else
        (filter-config-tire-sku-candidates a-config 
                                           (rest candidates))])]))





      
;; legal-configuration?: config -> boolean
;; Returns true if the global configuration given is a legal one.
(define (legal-configuration? a-config)
  (and (rule-extra? a-config)
       (rule-shoes? a-config)
       (rule-tire? a-config)))


;; implies: boolean boolean -> boolean
(define (implies x y)
  (cond
    [x y]
    [else
     true]))



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
     (string-append "Illegal configuration. "
                    (broken-rule-message "Extra" (rule-extra? a-config))
                    (broken-rule-message "Shoes" (rule-shoes? a-config))
                    (broken-rule-message "Tires" (rule-tire? a-config))
                    "]")]))
                    

;; broken-rule-message: string boolean -> string
(define (broken-rule-message category ok?)
  (cond
    [ok? ""]
    [else 
     (string-append category " has a problem. ")]))


;; config-pedal-sku: config -> config
(define (config-pedal-sku a-config)
  (pedal-sku (config-pedal a-config)))


;; update-config-pedal-sku: config string -> config
(define (update-config-pedal-sku a-config an-sku)
  (update (pedal-sku (config-pedal a-config)) an-sku))


;; config-pedal-pedaltype: config -> string
(define (config-pedal-pedaltype a-config)
  (pedal-pedaltype (config-pedal a-config)))


(define pedal-gui
  (box-group
   "Pedal"
   (col (row "SKU" 
             (drop-down config-pedal-sku PEDALS update-config-pedal-sku))
        (row "Pedal type" 
             (message config-pedal-pedaltype)))))



;; config-shoes-sku: config -> string
(define (config-shoes-sku a-config)
  (shoes-sku (config-shoes a-config)))


;; update-config-shoes-sku: config string -> string
(define (update-config-shoes-sku a-config a-sku)
  (update (shoes-sku (config-shoes a-config)) a-sku))


(define shoes-gui
   (box-group 
    "Shoes"
    (col (row "SKU"
              (drop-down config-shoes-sku 
                         config-shoes-sku-choices
                         update-config-shoes-sku)))))


;; config-extra-carrier?: config -> boolean
(define (config-extra-carrier? a-config)
  (extra-carrier? (config-extra a-config)))


;; config-extra-mudguard?: config -> boolean
(define (config-extra-mudguard? a-config)
  (extra-mudguard? (config-extra a-config)))


;; config-extra-lock?: config -> boolean
(define (config-extra-lock? a-config)
  (extra-lock? (config-extra a-config)))


;; config-extra-pump?: config -> boolean
(define (config-extra-pump? a-config)
  (extra-pump? (config-extra a-config)))


;; config-extra-bottle?: config -> boolean
(define (config-extra-bottle? a-config)
  (extra-bottle? (config-extra a-config)))


;; update-config-extra-carrier?: config boolean -> config
(define (update-config-extra-carrier? a-config a-bool)
  (update (extra-carrier? (config-extra a-config)) a-bool))


;; update-config-extra-mudguard?: config -> boolean
(define (update-config-extra-mudguard? a-config a-bool)
  (update (extra-mudguard? (config-extra a-config)) a-bool))


;; config-extra-lock?: config -> boolean
(define (update-config-extra-lock? a-config a-bool)
  (update (extra-lock? (config-extra a-config)) a-bool))


;; config-extra-pump?: config -> boolean
(define (update-config-extra-pump? a-config a-bool)
  (update (extra-pump? (config-extra a-config)) a-bool))


;; config-extra-bottle?: config -> boolean
(define (update-config-extra-bottle? a-config a-bool)
  (update (extra-bottle? (config-extra a-config)) a-bool))


(define extra-gui
  (box-group 
   "Extra Accessories"
   (col
    (row "Carrier?" 
         (checkbox config-extra-carrier? update-config-extra-carrier?))
    (row "Mudguard?" 
         (checkbox config-extra-mudguard? update-config-extra-mudguard?))
    (row "Lock?" 
         (checkbox config-extra-lock? update-config-extra-lock?))
    (row "Pump?"
         (checkbox config-extra-pump? update-config-extra-pump?))
    (row "Bottle?" 
         (checkbox config-extra-bottle? update-config-extra-bottle?)))))


;; config-rim-sku: config -> string
(define (config-rim-sku a-config)
  (rim-sku (config-rim a-config)))

;; update-config-rim-sku: config string -> config
(define (update-config-rim-sku a-config a-sku)
  (update (rim-sku (config-rim a-config)) a-sku))

;; config-rim-height: config -> string
(define (config-rim-height a-config)
  (rim-height (config-rim a-config)))

;; config-rim-width: config -> string
(define (config-rim-width a-config)
  (rim-width (config-rim a-config)))


(define rim-gui
  (box-group "Rims"
             (col (row "SKU"
                       (drop-down config-rim-sku RIMS update-config-rim-sku))
                  (row "Height"
                       (message config-rim-height))
                  (row "Width" 
                       (message config-rim-width))))) 


;; config-tire-sku: config -> string
(define (config-tire-sku a-config)
  (tire-sku (config-tire a-config)))

;; update-config-tire-sku: config string -> config
(define (update-config-tire-sku a-config a-sku)
  (update (tire-sku (config-tire a-config)) a-sku))


(define tire-gui
  (box-group 
   "Tires"
   (col (row "SKU"
             (drop-down config-tire-sku 
                        config-tire-sku-choices 
                        update-config-tire-sku)))))


(define main-gui
  (col (row (col pedal-gui shoes-gui
                 rim-gui tire-gui) 
            extra-gui)
       (message legal-configuration-status)))


(big-bang initial-config main-gui)