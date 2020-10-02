(defpackage #:scripts-asdf
  (:use :cl :asdf))

(in-package :scripts-asdf)


(defsystem scripts
  ;; :depends-on (:inferior-shell :uiop :inferior-shell)
  :name "misc scripts"
  :version "0.0.0.0"
  :maintainer "mariari"
  :author     "mariari"
  :description "scripts I've made to make my life easier"
  :pathname "scripts/"
  :components ((:file "./slex-choice")
               ;; (:file "./cache-fstar-source")
               ))
