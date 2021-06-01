(defpackage #:scripts-asdf
  (:use :cl :asdf))

(in-package :scripts-asdf)


(defsystem scripts
  :depends-on (:inferior-shell :uiop :cl-json :slex)
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

(defsystem slex
  :depends-on (:let-over-lambda)
  :name "misc scripts"
  :version "0.0.0.0"
  :maintainer "mariari"
  :author     "mariari"
  :description "scripts I've made to make my life easier"
  :pathname "scripts/slex-choice/src"
  :components ((:file "race-role-map" :depends-on ("bi-table"))
               (:file "entity" :depends-on ("race-role-map"))
               (:file "bi-table")))

(asdf:defsystem :slex/test
  :depends-on (:slex :fiveam)
  :description "testing slex"
  :pathname "scripts/slex-choice/test"
  :components ((:file "testpkg")
               (:file "race-role" :depends-on ("testpkg"))
               (:file "entity"    :depends-on ("testpkg"))
               (:file "run-tests" :depends-on ("race-role" "entity")))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :slex-test :run-tests)))
