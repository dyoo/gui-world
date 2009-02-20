#lang scheme/base
(require "private/gui-world.ss"
         "private/accessor.ss")

(printf "gui world instantiated~n")

(provide (all-from-out "private/gui-world.ss")
         
         get/accessor
         update/accessor
         chain-accessors)