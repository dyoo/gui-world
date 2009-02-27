#lang scheme/base
(require "private/gui-world.ss"
         "private/accessor.ss"
         "private/prim.ss"
         "private/define-graph-function.ss"
         "private/arrow.ss"
         htdp/image
         (only-in htdp/world key=? key-event?))

#;(printf "gui world instantiated~n")



(provide-primitive big-bang)

(provide-higher-order-primitive on-tick (_ on-tick-handler))

(provide-higher-order-primitive on-key-event (key-handler))

(provide-primitive elt?)

(provide-primitives col row)

;; NOTE: We're doing this (having pairs of functions, one with and one 
;; without the enabled argument) because provide-higher-order-primitive doesn't
;; support optional arguments.
;;
;; One other deviation that I haven't figured out how to get around yet is
;; that higher-order values are required to be functions in beginner level.  But the design
;; of gui-world asks that we allow primitive values there too for convenience.  Argh.
;;
;; TODO: we need to figure out how to allow non-higher-order values in higher-order
;; position to fit the original design of gui-world.
(provide-maybe-higher-order-primitive message (val-f))
(provide-maybe-higher-order-primitive button (val-f callback))
(provide-maybe-higher-order-primitive button/enabled (val-f callback enabled?-f))
(provide-maybe-higher-order-primitive slider (val-f min-f max-f callback))
(provide-maybe-higher-order-primitive slider/enabled (val-f min-f max-f callback enabled?-f))
(provide-maybe-higher-order-primitive drop-down (val-f choices-f callback))
(provide-maybe-higher-order-primitive drop-down/enabled (val-f choices-f callback enabled?-f))
(provide-maybe-higher-order-primitive text-field (val-f callback))
(provide-maybe-higher-order-primitive text-field/enabled (val-f callback enabled?-f))
(provide-maybe-higher-order-primitive checkbox (label-f val-f callback))
(provide-maybe-higher-order-primitive checkbox/enabled (label-f val-f callback enabled?-f))
(provide-maybe-higher-order-primitive canvas (scene-f))
(provide-maybe-higher-order-primitive canvas/callback (scene-f callback))
(provide-maybe-higher-order-primitive box-group (val-f _))
(provide-maybe-higher-order-primitive box-group/enabled (val-f _ enabled?-f))
(provide-higher-order-primitive project/inject/gui (_ projection-f injection-f))

(provide ;; Other helpers
         define-updaters
         update
         with-getter/updater
         
         update-color-red update-color-green update-color-blue
         update-posn-x update-posn-y
         color-red-accessor color-green-accessor color-blue-accessor
         posn-x-accessor posn-y-accessor
         
         
         random-choice
         place-image
         empty-scene
         nw:rectangle
         (all-from-out htdp/image)
         (all-from-out "private/arrow.ss"))



(provide get/accessor
         update/accessor
         chain-accessors)

(provide define-graph-function)

(provide key=? key-event?)