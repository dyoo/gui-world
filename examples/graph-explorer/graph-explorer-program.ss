#lang scheme
(require "../../gui-world.ss"
         "graph-explorer.ss")

(define last-world
  (gui-big-bang initial-world view))