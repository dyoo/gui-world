;; The first three lines of this file were inserted by DrScheme. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname kathi-cell-allversions) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;; Cell phone example : basic configuration into constraint solving
;; Kathi Fisler
;; October 30, 2008

(require "../../gui-world.ss")

;; THE MODEL

; A plan is (make-plan string number boolean boolean boolean boolean list[number])
(define-struct plan (profile minutes free-weekends? free-text-msg? data? family? free-calls-to))

(define-updaters plan)

(define emptybase (make-plan "None" 0 false false false false empty))
(define economybase (make-plan "Economy" 60 false false false false empty))
(define standardbase (make-plan "Standard" 200 true false false false empty))
(define deluxebase (make-plan "Deluxe" 800 true true true false empty))

(define ALL-BASES (list emptybase economybase standardbase deluxebase))

;; get-base-names : list[plan] -> list[string]
;; produces list of profile names for all plans in list
(define (get-base-names alob)
  (cond [(empty? alob) empty]
        [(cons? alob) (cons (plan-profile (first alob)) 
                            (get-base-names (rest alob)))]))

(define ALL-BASE-NAMES (get-base-names ALL-BASES))

;; find-base : string list[plan] -> plan
;; produces plan with given string as profile field, or false if no such plan
;;   want to assume that basename is represented in alob
(define (find-base basename alob)
  (cond [(empty? alob) false] ;; really want error here
        [(cons? alob)
         (cond [(string=? basename (plan-profile (first alob))) (first alob)]
               [else (find-base basename (rest alob))])]))

(define (find-base-by-name currplan basename)
  (find-base basename ALL-BASES))

#|
- account profiles:
    * economy -- few minutes, no extras
    * standard -- more minutes, free weekends
    * deluxe -- even more minutes, free weekends, free data (incl messaging)

Constraints:
  - if choose profile, can't change features included in a profile
  - text messaging is included in the enhanced data package
  - can't have both family plan and free frequent callers
|#

;; VERSION 1 -- basic gui to configure, all choices legal

(define gui1
  (row
   (col 
    (box-group 
     "Profile" 
     (drop-down plan-profile ALL-BASE-NAMES find-base-by-name))
    (box-group
     "Customize"
     ;; when plan changes, the text field contents are not updating
     (row "Number of minutes" (text-field plan-minutes update-plan-minutes)))
    (box-group 
     "Options" 
     (col (checkbox "Add free weekends" plan-free-weekends? update-plan-free-weekends?)
          (checkbox "Add free text messaging" plan-free-text-msg? update-plan-free-text-msg?)
          (checkbox "Add data plan" plan-data? update-plan-data?)
          (checkbox "Make family plan" plan-family? update-plan-family?)
          )))
   (col (box-group "Summary"
                   (message "Still need to fill in")))))

(define last-world
  (gui-big-bang emptybase gui1))

;; VERSION 2 -- detect illegal configurations, use naive cond statements

;; VERSION 3 -- detect illegal configurations, use formula language

;; VERSION 4 -- enable/disable GUI elements based on choices/formulas

