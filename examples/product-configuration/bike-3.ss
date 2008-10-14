#lang scheme/base
(require "../../gui-world.ss"
         scheme/list)



;; Very simple repair algorithm: we only handle rules of the form
;; var1 in X => var2 in Y
;; where X and Y are (listof string), and var1 and var2 are strings.
;;
;; FIXME: assumption is that rules won't lead us to infinite recursion,
;; and the rules don't fight each other. 
;; This assumption might be unreasonable.
(define-struct rule:implies (lhs    ;; expr
                             rhs))  ;; expr
(define-struct rule:in (var         ;; string
                        dom))       ;; (listof string-or-boolean)


;; An env is a (string, string) hash.

;; env-ref: env string -> (or string #f)
(define (env-ref an-env a-var)
  (hash-ref an-env a-var #f))

;; env-update: env string string -> env
(define (env-update an-env a-var a-val)
  (hash-set an-env a-var a-val))

(define empty-env (make-immutable-hash (list)))


;; rule-satisfied?: rule env -> boolean
;; Returns true if the rule is satisfied by the given environment.
(define (rule-satisfied? a-rule an-env)
  (cond [(rule:implies? a-rule)
         (if (rule-satisfied? (rule:implies-lhs a-rule) an-env)
             (rule-satisfied? (rule:implies-rhs a-rule) an-env)
             #t)]
        [(rule:in? a-rule)
         (and (member (env-ref an-env (rule:in-var a-rule))
                      (rule:in-dom a-rule))
              #t)]))


;; repair-with-rule: rule env -> (listof env)
;; Given a rule and an env, returns a list of environments that satisfy that rule.
(define (repair-with-rule a-rule an-env)
  (cond
    [(rule:implies? a-rule)
     (cond
       [(rule-satisfied? a-rule an-env)
        (list an-env)]
       [else
        (repair-with-rule (rule:implies-rhs a-rule) an-env)])]
    [(rule:in? a-rule)
     (cond
       [(rule-satisfied? a-rule an-env)
        (list an-env)]
       [else
        (map (lambda (elt) 
               (env-update an-env 
                           (rule:in-var a-rule)
                           elt))
             (rule:in-dom a-rule))])])) 


;; repair-with-rules: (listof rule) env -> (listof env)
(define (repair-with-rules rules an-env)
  (cond
    [(empty? rules)
     (list an-env)]
    [else
     (apply append
            (map (lambda (an-env)
                   (repair-with-rules (rest rules) an-env))
                 (repair-with-rule (first rules) an-env)))]))


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
                      ))

(define-updaters extra)
(define initial-extra (make-extra #f #f #f #f #f))

;; add-extra-to-env: extra env -> env
(define (add-extra-to-env an-extra an-env)
  (foldl (lambda (name accessor an-env)
           (env-update an-env name (accessor an-extra)))
         an-env
         (list "carrier?" "mudguard?" "lock?" "pump?" "bottle?")
         (list extra-carrier? extra-mudguard? extra-lock? extra-pump? extra-bottle?)))

;; extra-extra-from-env: env -> extra
(define (extract-extra-from-env an-env)
  (make-extra (env-ref an-env "carrier?")
              (env-ref an-env "mudguard?")
              (env-ref an-env "lock?")
              (env-ref an-env "pump?")
              (env-ref an-env "bottle?")))
              



(define rule:carrier-needs-mudguard
  (make-rule:implies (make-rule:in "carrier?" (list #t))
                     (make-rule:in "mudguard?" (list #t))))

(define rule:pump-bottle-exclusive-1
  (make-rule:implies (make-rule:in "pump?" (list #t))
                     (make-rule:in "bottle?" (list #f))))

(define rule:pump-bottle-exclusive-2
  (make-rule:implies (make-rule:in "bottle?" (list #t))
                     (make-rule:in "pump?" (list #f))))



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


;; add-pedal-to-env: pedal env -> env
(define (add-pedal-to-env a-pedal an-env)
  (env-update
   (env-update an-env "sku" (pedal-sku a-pedal))
   "pedaltype"
   (pedal-pedaltype a-pedal)))


;; extract-pedal-from-env: env -> pedal
(define (extract-pedal-from-env an-env)
  (make-pedal (env-ref an-env "sku")
              (env-ref an-env "pedaltype")))


(define rule:legal-pedal-config-1
  (make-rule:implies (make-rule:in "sku" (list "PD 6600"))
                     (make-rule:in "pedaltype" (list "SPD"))))

(define rule:legal-pedal-config-2
  (make-rule:implies (make-rule:in "sku" (list "PD 5500"))
                     (make-rule:in "pedaltype" (list "SPD"))))

(define rule:legal-pedal-config-3
  (make-rule:implies (make-rule:in "sku" (list "PD M545"))
                     (make-rule:in "pedaltype" (list "Clip"))))

(define rule:legal-pedal-config-4
  (make-rule:implies (make-rule:in "sku" (list "PD M434"))
                     (make-rule:in "pedaltype" (list "Clip"))))

(define rule:legal-pedal-config-5
  (make-rule:implies (make-rule:in "sku" (list "Campagnolo Record"))
                     (make-rule:in "pedaltype" (list "SPD"))))

(define rule:legal-pedal-config-6
  (make-rule:implies (make-rule:in "sku" (list "Campagnolo Chorus"))
                     (make-rule:in "pedaltype" (list "SPD"))))

(define rule:legal-pedal-config-7
  (make-rule:implies (make-rule:in "sku" (list "PD C105"))
                     (make-rule:in "pedaltype" (list "Standard"))))

(define rule:legal-pedal-config-8
  (make-rule:implies (make-rule:in "sku" (list "Black Plastic"))
                     (make-rule:in "pedaltype" (list "Standard"))))

(define rule:legal-pedal-config-9
  (make-rule:implies (make-rule:in "sku" (list "PD C101"))
                     (make-rule:in "pedaltype" (list "Standard"))))

(define rule:legal-pedal-config-10
  (make-rule:implies (make-rule:in "pedaltype" (list "Standard"))
                     (make-rule:in "sku" (list "PD C105" 
                                               "Black Plastic"
                                               "PD C101"))))
(define rule:legal-pedal-config-11
  (make-rule:implies (make-rule:in "pedaltype" (list "SPD"))
                     (make-rule:in "sku" (list "PD 6600"
                                               "PD 5500"
                                               "Campagnolo Record"
                                               "Campagnolo Chorus"))))
(define rule:legal-pedal-config-12
  (make-rule:implies (make-rule:in "pedaltype" (list "Clip"))
                     (make-rule:in "sku" (list "PD M545"
                                               "PD M434"))))


(define initial-world
  (make-config initial-extra
               initial-pedal))

;
;
;;; checkbox/rule: (gvalueof boolean) (gcallbackof boolean) -> checkbox
;;; Creates a checkbox that is enabled so long as a change to the checkbox still leads to
;;; a valid configuration.
;(define (checkbox/rule val-f callback)
;  (local [(define (ok-to-change? a-world)
;            (legal-configuration? (callback a-world (not (val-f a-world)))))]
;    (checkbox val-f callback ok-to-change?)))
;
;
;;; drop-down/rule: (gvalueof string) (gvalueof (listof string)) (gcallbackof string) -> drop-down
;;; Creates a dropdown whose choices are limited to the ones that lead to a valid configuration.
;(define (drop-down/rule val-f choices-f callback)
;  (local [(define (good-choices a-world)
;            (filter (lambda (a-choice)
;                      (legal-configuration? (callback a-world a-choice)))
;                    (choices-f a-world)))]
;    (drop-down val-f good-choices callback)))
;
;
;;; The GUI.
;(define extra-gui
;  (local [(define (cb getter updater)
;            (checkbox/rule (project getter config-extra)
;                           (project/inject updater config-extra update-config-extra)))]
;    (box-group "Extra Accessories"
;               (col
;                (row "Carrier?" (cb extra-carrier? update-extra-carrier?)) 
;                (row "Mudguard?" (cb extra-mudguard? update-extra-mudguard?))
;                (row "Lock?" (cb extra-lock? update-extra-lock?))
;                (row "Pump?" (cb extra-pump? update-extra-pump?))
;                (row "Bottle?" (cb extra-bottle? update-extra-bottle?))))))
;                
;
;(define pedal-gui
;  (local [(define (dd getter choices updater)
;            (drop-down/rule (project getter config-pedal)
;                            (lambda (a-world) choices)
;                            (project/inject updater config-pedal update-config-pedal)))]
;    (box-group "Pedal"
;               (col (row "SKU" (dd pedal-sku PEDALS update-pedal-sku))
;                    (row "Pedal type" (dd pedal-pedaltype PEDAL-TYPES update-pedal-pedaltype))))))
;
;
;(define main-gui
;  (col (row pedal-gui extra-gui)
;       (message legal-configuration-status)))
;
;
;(big-bang initial-world main-gui)