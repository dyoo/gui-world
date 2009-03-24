#lang scheme/base
(require scheme/class
         scheme/gui/base)

(require (prefix-in slideshow: slideshow/pict))

;; scale-image-snip: image-snip number number -> image-snip
;; Scales an image snip
(define (scale-image-snip an-image-snip w-factor h-factor)
  (let* ([bm (send an-image-snip get-bitmap)]
         [a-pict (slideshow:frame
                  (slideshow:scale (slideshow:bitmap bm)
                                   w-factor
                                   h-factor))]
         [shrunk-bm (make-object bitmap% 
                      (add1 (inexact->exact (ceiling (slideshow:pict-width a-pict))))
                      (add1 (inexact->exact (ceiling (slideshow:pict-height a-pict)))))]
         [shrunk-bm-dc (new bitmap-dc% [bitmap shrunk-bm])]
         [a-snip (new image-snip%)])
    (slideshow:draw-pict a-pict shrunk-bm-dc 0 0)
    (send shrunk-bm-dc set-bitmap #f)
    (send a-snip set-bitmap shrunk-bm)
    a-snip))

(provide scale-image-snip)