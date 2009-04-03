;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cell-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Cell phone plan configuration
(require "../../gui-world.ss")

;; A plan is a base plan and accessories.
(define-struct plan (base accessories))
(define-updaters plan)

;; A base is a:
(define-struct base (name
                     minutes             ;; number
                     price               ;; number
                     weekends-included?  ;; boolean
                     friends-included?   ;; boolean
                     max-lines))         ;; number
(define-updaters base)


;; Accessories allow us to add extensions to the base plan.
;; An accessories is a:
(define-struct accessories (weekends?                     ;; boolean
                            friends?                      ;; boolean
                            extra-lines))                 ;; number
(define-updaters accessories)
(define NO-ACCESSORIES (make-accessories false false 0))


;; Examples of base plans that we will present to the user.
(define SINGLE-BASES 
  (list (make-base "Economy single"
                   450
                   39.99
                   false
                   false
                   1)
        
        (make-base "Moderate single"
                   900
                   49.99
                   true
                   false
                   1)
        
        (make-base "Heavy single"
                   1350
                   59.99
                   true
                   true
                   1)))

(define SHARED-BASES
  (list (make-base "Economy family"
                   550
                   59.99
                   false
                   false
                   3)
        
        (make-base "Moderate family"
                   700
                   69.99
                   true
                   false
                   5)
        
        (make-base "Heavy family"
                   1400
                   89.99
                   true
                   true
                   5)))


;; We keep a list of all base plans.
(define ALL-BASES (append SINGLE-BASES SHARED-BASES))




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



;; plan-name: plan -> string
;; Consumes a plan, and produces the base name.
(define (plan-name a-plan)
  (base-name (plan-base a-plan)))


;; plan-price: plan -> number
;; Consumes a plan and produces its overall price.
(define (plan-price a-plan)
  (+ (base-price (plan-base a-plan))
     (accessories-price (plan-accessories a-plan))))


;; plan-price-string: plan -> string
(define (plan-price-string a-plan)
  (string-append (number->string 
                  (dollars (plan-price a-plan)))
                 "."
                 (cents->string (cents (plan-price a-plan)))))


;; dollars: number -> number
(define (dollars a-num)
  (floor a-num))


;; cents: number -> number
(define (cents a-num)
  (floor (* (- a-num (floor a-num))
            100)))

;; cents->string: number -> string
(define (cents->string cents)
  (cond
    [(< cents 10)
     (string-append "0" (number->string cents))]
    [else
     (number->string cents)]))


;; plan-lines: plan -> number
;; Consumes a plan and produces how many lines it has.
(define (plan-lines a-plan)
  (+ 1 (accessories-extra-lines (plan-accessories a-plan))))


;; plan-max-lines: plan -> number
(define (plan-max-lines a-plan)
  (base-max-lines (plan-base a-plan)))


;; update-plan-lines: plan number -> plan
(define (update-plan-lines a-plan lines)
  (update (accessories-extra-lines (plan-accessories a-plan))
          (sub1 lines)))


;; plan-minutes: plan -> number
;; Consumes a plan and produces how many minutes it has.
(define (plan-minutes a-plan)
  (base-minutes (plan-base a-plan)))


;; plan-has-unlimited-weekends?: plan -> boolean
(define (plan-has-unlimited-weekends? a-plan)
  (or (base-weekends-included? (plan-base a-plan))
      (accessories-weekends? (plan-accessories a-plan))))

;; plan-has-unlimited-friends?: plan -> boolean
(define (plan-has-unlimited-friends? a-plan)
  (or (base-friends-included? (plan-base a-plan))
      (accessories-friends? (plan-accessories a-plan))))




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



;; update-plan-by-name.
;; Given a change in name, update the plan appropriately.
;; If the plan changes, clear off the accessories list
;; of accessories that are incompatible with the plan.
(define (update-plan-by-name a-plan a-plan-name)
  (cond [(string=? (plan-name a-plan) a-plan-name)
         a-plan]
        [else
         (clear-accessories-provided-by-base 
          (update (plan-base a-plan) 
                  (find-base-by-name a-plan-name ALL-BASES)))]))


;; clear-accessories-provided-by-base: plan -> plan
;; Given a plan, clear out any of the accessories that are
;; already included in the base plan.
(define (clear-accessories-provided-by-base a-plan)
  (clear-weekends-if-base-provided
   (clear-friends-if-base-provided a-plan)))


;; clear-weekends-if-base-provided: plan -> plan
(define (clear-weekends-if-base-provided a-plan)
  (cond [(base-weekends-included? (plan-base a-plan))
         (update (accessories-weekends? (plan-accessories a-plan)) false)]
        [else
         a-plan]))


;; clear-friends-if-base-provided: plan -> plan
(define (clear-friends-if-base-provided a-plan)
  (cond [(base-friends-included? (plan-base a-plan))
         (update (accessories-friends? (plan-accessories a-plan)) false)]
        [else
         a-plan]))






;; base-names: (listof base) -> (listof String)
(define (base-names bases)
  (cond [(empty? bases)
         empty]
        [else
         (cons (base-name (first bases))
               (base-names (rest bases)))]))     


;; find-base-by-name: string (listof base) -> base
(define (find-base-by-name a-base-name bases)
  (cond
    [(empty? bases)
     (error 'find-base-by-name "Couldn't find it.")]
    [(string=? a-base-name (base-name (first bases)))
     (first bases)]
    [else
     (find-base-by-name a-base-name (rest bases))]))


(define ALL-BASE-NAMES (base-names ALL-BASES))



(define base-plan-gui 
  (row "Base plan"
       (drop-down plan-name
                  ALL-BASE-NAMES
                  update-plan-by-name)))



;; plan-added-weekends?: plan -> boolean
;; Consumes a plan and produces true if we've added unlimited weekends.
(define (plan-added-weekends? a-plan)
  (accessories-weekends? (plan-accessories a-plan)))


;; update-plan-added-weekends?: plan boolean -> plan
(define (update-plan-added-weekends? a-plan a-bool)
  (revert-to-old-plan-if-illegal
   (update (accessories-weekends? (plan-accessories a-plan))
           a-bool)
   a-plan))


;; plan-added-friends?: plan -> boolean
;; Consumes a plan and produces true if we've added unlimited friends.
(define (plan-added-friends? a-plan)
  (accessories-friends? (plan-accessories a-plan)))


;; update-plan-added-friends?: plan boolean -> plan
(define (update-plan-added-friends? a-plan a-bool)
  (revert-to-old-plan-if-illegal
   (update (accessories-friends? (plan-accessories a-plan))
           a-bool)
   a-plan))


;; revert-to-old-plan-if-illegal: plan plan -> plan
;; If the new-plan is legal, produces it.  Otherwise, produces the
;; old plan.
(define (revert-to-old-plan-if-illegal new-plan old-plan)
  (cond
    [(legal-plan? new-plan)
     new-plan]
    [else
     old-plan]))



;; current-line-choice: plan -> string
(define (current-line-choice a-plan)
  (number->string (plan-lines a-plan)))

;; update-line-choice: plan string -> plan
(define (update-line-choice a-plan a-choice)
  (update-plan-lines a-plan (string->number a-choice)))

;; line-choices: plan -> (listof string)
;; Returns a list of line choices up to the maximum list of choices.
(define (line-choices a-plan)
  (string-range (plan-max-lines a-plan)))

;; string-range: number -> string
;; Produces a list of strings from 1 .. n
(define (string-range n)
  (cond
    [(= n 0)
     empty]
    [else
     (append (string-range (sub1 n))
             (list (number->string n)))]))



;; weekends-checkbox-enabled?: plan -> boolean
;; Produces true if the gui should enable the weekends accessories check box.
(define (weekends-checkbox-enabled? a-plan)
  (not (base-weekends-included? (plan-base a-plan))))

;; friends-checkbox-enabled?: plan -> boolean
;; Produces true if the gui should enable the friends accessories check box.
(define (friends-checkbox-enabled? a-plan)
  (not (base-friends-included? (plan-base a-plan))))


(define accessories-gui 
  (box-group 
   "Accessories"
   (col
    (row "Number of lines" 
         (drop-down current-line-choice line-choices update-line-choice))
    (checkbox/enabled "Add unlimited weekends" plan-added-weekends? update-plan-added-weekends? weekends-checkbox-enabled?)
    (checkbox/enabled "Add unlimited friends" plan-added-friends? update-plan-added-friends? friends-checkbox-enabled?))))
    


(define a-gui (col (box-group "Configuration"
                              (col base-plan-gui
                                   accessories-gui))
                   (box-group "Summary"
                              (col
                               (row "Plan name: " (message plan-name))
                               (row "Minutes: " (message plan-minutes))
                               (row "Number of lines: " (message plan-lines))
                               (row "Has unlimited weekends? " 
                                    (message plan-has-unlimited-weekends?))
                               (row "Has unlimited calls to friends? "
                                    (message plan-has-unlimited-friends?))
                               (row "Price: " (message plan-price-string))))))

(define last-world
  (gui-big-bang initial-world a-gui))