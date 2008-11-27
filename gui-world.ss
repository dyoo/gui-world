#lang scheme/base
(require "private/gui-world.ss"
         "private/accessor.ss")

(provide (all-from-out "private/gui-world.ss")
         
         get/accessor
         update/accessor
         chain-accessors)