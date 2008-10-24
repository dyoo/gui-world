;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cell-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Cell phone plan configuration
(require "../../gui-world.ss")

;; A plan is a base plan and accessories.
(define-struct plan (base accessories))


;; A base is either a single-base, or a shared-base.
;;
;;
;; A single-base-plan is a
(define-struct single-base (name
                            minutes             ;; number
                            price               ;; number
                            weekends-included?  ;; boolean
                            friends-included?)) ;; boolean
;; A single-base doesn't allow for additional lines.


;; A shared-base is a:
(define-struct shared-base (name
                            minutes             ;; number
                            price               ;; number
                            weekends-included?  ;; boolean
                            friends-included?   ;; boolean
                            max-lines))  ;; number
;; It allows for multiple lines.



;; Accessories allow us to add extensions to the base plan.
;; An accessories is a:
(define-struct accessories (weekends?                     ;; boolean
                            friends?                      ;; boolean
                            extra-lines))                 ;; number
(define NO-ACCESSORIES (make-accessories false false 0))





;; Examples of base plans that we will present to the user.
(define SINGLE-BASES 
  (list (make-single-base "Economy single"
                               450
                               39.99
                               false
                               false)
        
        (make-single-base "Moderate single"
                               900
                               49.99
                               true
                               false)

        (make-single-base "Heavy single"
                               1350
                               59.99
                               true
                               true)))

(define SHARED-BASES
  (list (make-shared-base "Small family"
                               550
                               59.99
                               false
                               false
                               3)
        
        (make-shared-base "Moderate family"
                               700
                               69.99
                               true
                               false
                               5)
        
        (make-shared-base "Heavy family"
                               1400
                               89.99
                               true
                               true
                               5)))
  


;; base-name: base -> string
;; Consumes a base and produces its name.
(define (base-name a-base)
  (cond [(single-base? a-base)
         (single-base-name a-base)]
        [(shared-base? a-base)
         (shared-base-name a-base)]))
      

;; base-minutes: base -> number
;; Consumes a base plan and produces its minutes.
(define (base-minutes a-base)
  (cond
    [(single-base? a-base)
     (single-base-minutes a-base)]
    [(shared-base? a-base)
     (shared-base-minutes a-base)]))


;; base-price: base -> number
;; Consumes a base plan and produces its price.
(define (base-price a-base)
  (cond
    [(single-base? a-base)
     (single-base-price a-base)]
    [(shared-base? a-base)
     (shared-base-price a-base)]))


;; base-weekends-included?: base -> boolean
;; Consumes a base plan and produces if weekends are already included.
(define (base-weekends-included? a-base)
  (cond
    [(single-base? a-base)
     (single-base-weekends-included? a-base)]
    [(shared-base? a-base)
     (shared-base-weekends-included? a-base)]))


;; base-friends-included?: base -> boolean
;; Consumes a base plan and produces if friends are already included.
(define (base-friends-included? a-base)
  (cond
    [(single-base? a-base)
     (single-base-friends-included? a-base)]
    [(shared-base? a-base)
     (shared-base-friends-included? a-base)]))


;; base-max-lines: base -> number
;; Consumes a base and produces how many lines it will allow.
(define (base-max-lines a-base)
  (cond [(single-base? a-base)
         1]
        [(shared-base? a-base)
         (shared-base-max-lines a-base)]))


;; accessories-price: accessories -> number
;; Consumes an accessories and produces its price.
(define (accessories-price an-accessories)
  (+ (cond [(accessories-weekends? an-accessories)
            9.99]
           [else
            0])
     (cond [(accessories-friends? an-accessories)
            9.99]
           [else
            0])
     (* 8.99 (accessories-extra-lines an-accessories))))
        

;; plan-price: plan -> number
;; Consumes a plan and produces its overall price.
(define (plan-price a-plan)
  (+ (base-price (plan-base a-plan))
     (accessories-price (plan-accessories a-plan))))


;; plan-lines: plan -> number
;; Consumes a plan and produces how many lines it has.
(define (plan-lines a-plan)
  (+ 1 (accessories-extra-lines (plan-accessories a-plan))))



;; We have two simple constraints on our model:
;;
;; The number of lines in a plan should be less than or equal to the maximum
;; allowed.
;;
;; If weekends or friends are included in a plan already, accessories must
;; not allow additions.


;; legal-plan?: plan -> boolean
;; Consumes a plan and produces true if the plan satisfies 
;; the simple constraint described above.
(define (legal-plan? a-plan)
  (and (<= (plan-lines a-plan)
           (base-max-lines (plan-base a-plan)))
       (implies (base-weekends-included? (plan-base a-plan))
                (not (accessories-weekends? 
                      (plan-accessories a-plan))))

       (implies (base-friends-included? (plan-base a-plan))
                (not (accessories-friends? 
                      (plan-accessories a-plan))))))
  

;; implies: boolean boolean -> boolean
;; logical implication
(define (implies x y)
  (or (not x) y))


(check-expect (legal-plan? 
               (make-plan (first SINGLE-BASES)
                          NO-ACCESSORIES))
              true)

(check-expect (legal-plan? 
               (make-plan (first SINGLE-BASES)
                          (make-accessories false false 1)))
              false)

(check-expect (legal-plan? 
               (make-plan (first SHARED-BASES)
                          (make-accessories false false 1)))
              true)

(check-expect (legal-plan?
               (make-plan (third SINGLE-BASES)
                          NO-ACCESSORIES))
              true)

(check-expect (legal-plan?
               (make-plan (third SINGLE-BASES)
                          (make-accessories true false 0)))
              false)



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
;                     ;;  ;;  

;; Our world is a plan.
(define initial-world (make-plan (first SINGLE-BASES)
                                 NO-ACCESSORIES))

; Features of the gui:
;
; If a base plan includes an accessory, we should not allow
; the user to add the accessory since it will already be included
; with the price.



(define base-plan-gui (col "Select a base plan"))

(define (number-of-lines a-world)
  ...)

(define accessories-gui (col "Select your accessories"
                             #;(box-group "Number of lines" )))

(define summary-gui (col "Summary and total costs."))


(define a-gui (row (col base-plan-gui
                        accessories-gui)
                   summary-gui))
#;(big-bang 0 a-gui)