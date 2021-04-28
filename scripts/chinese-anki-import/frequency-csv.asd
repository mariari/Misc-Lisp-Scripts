(defpackage #:freq-csv
  (:use :cl :asdf))

(in-package :freq-csv)


(defsystem freq-csv
  :depends-on (:inferior-shell :uiop :fset :cl-ppcre)
  :name "Freuqnecy CSV"
  :version "0.0.0.0"
  :maintainer "mariari"
  :author     "mariari"
  :description "a script to convert pdf, epub, or text files into chinese frequency lists"
  :pathname "src/"
  :components ((:file "./tokenizer")
               (:file "./format-changer")
               (:file "./convert" :depends-on ("./tokenizer" "./format-changer"))))
