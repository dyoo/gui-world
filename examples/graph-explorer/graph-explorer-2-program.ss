#lang scheme
(require "../../gui-world.ss"
         "graph-explorer-2.ss")

(define last-world
  (gui-big-bang initial-world view))