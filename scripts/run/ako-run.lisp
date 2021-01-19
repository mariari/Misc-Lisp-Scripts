#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#

(load "/home/katya/Documents/Workspace/Lisp/fun/Misc-Lisp-Scripts/scripts.asd")
(ros:ensure-asdf)
(handler-bind ((warning #'muffle-warning)) (asdf:load-system :scripts))

(defun main (&rest argv)
  (apply #'scripts.akochan:akochan "mariari" argv))
