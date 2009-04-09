#lang scheme

(require "../gui-world.ss")


(gui-big-bang 42
          (pasteboard (list (message "hello world"))))