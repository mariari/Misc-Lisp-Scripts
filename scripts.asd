(defpackage #:scripts-asdf
  (:use :cl :asdf))

(in-package :scripts-asdf)


(defsystem scripts
  :depends-on (:inferior-shell :uiop :cl-json)
  :name "misc scripts"
  :version "0.0.0.0"
  :maintainer "mariari"
  :author     "mariari"
  :description "scripts I've made to make my life easier"
  :pathname "scripts/"
  :components ((:file "./slex-choice")
               (:file "./akochan")
               ;; (:file "./cache-fstar-source")
               ))
