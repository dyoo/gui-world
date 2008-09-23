#lang scheme/base

(require scheme/contract)


;; Views
(define-struct view ())
(define-struct (view:message view) (label*))
(define-struct (view:button view) (label* enabled?* callback))
(define-struct (view:slider view) (value* min* max* vcallback))
(define-struct (view:drop-down view) (value* choices* vcallback))
(define-struct (view:text-field view) (value* cursor-pos* vcallback))
(define-struct (view:image view) (scene*))
(define-struct (view:row view) (views))
(define-struct (view:col view) (views))
(define-struct (view:when test* view))