#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec ros -Q -- $0 "$@"
|#

(load "~/.sbcl-scripts")

(defun main (&rest argv)
  (apply #'scripts.akochan:akochan "mariari" argv))
