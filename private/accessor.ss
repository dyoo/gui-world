#lang scheme/base
(require scheme/contract)

;; Accessors
;;
;; (accessorof world X) 
;; where getter is (world -> X)
;;  and updater is (world X -> world)
(define-struct accessor (getter updater))


;; get/accessor: (accessorof world X) world -> X
;; Gets at substructure within a world.
(define (get/accessor acc a-world)
  ((accessor-getter acc) a-world))


;; update/accessor: (accessorof world X) world X -> world
;; updates a substructure within the world, using the provided accessor.
(define (update/accessor acc a-world a-val)
  ((accessor-updater acc) a-world a-val))


;; chain-accessors: (accessorof world-1 world-2) (accessorof world-2 X) -> (accessorof world-1 X)
(define (chain-accessors acc1 acc2)
  (make-accessor (lambda (a-world)
                   ((accessor-getter acc2)
                    ((accessor-getter acc1) a-world)))
                 
                 (lambda (a-world a-val)
                   ((accessor-updater acc1)
                    ((accessor-updater acc2)
                     ((accessor-getter acc1) a-world)
                     a-val)))))


(provide/contract [struct accessor ([getter (any/c . -> . any)]
                                    [updater (any/c any/c . -> . any)])]
                  [get/accessor (accessor? any/c . -> . any)]
                  [update/accessor (accessor? any/c any/c . -> . any)]
                  [chain-accessors (accessor? accessor? . -> . accessor?)])