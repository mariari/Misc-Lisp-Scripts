(defpackage #:misc-asdf
  (:use :cl :asdf))

(in-package :misc-asdf)


(defsystem misc
  :name "misc scripts"
  :version "0.0.0.0"
  :maintainer "mariari"
  :author     "mariari"
  :description "miscellaneous scripts"
  :long-description "various ideas I play around with and implement in LISP"

  :components ((:file "./CL/macros")
               (:file "./CL/functions"
                      :depends-on ("./CL/macros"))
               (:file "threadstest"
                      :depends-on ("./CL/functions"))))