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

  :components ((:file "./cl-user"
                      :depends-on ("./CL/stdlib/ref"
                                   "./CL/stdlib/lazy-cons"))
               (:file "./CL/macros")
               (:file "./CL/stdlib/list")
               (:file "./CL/stdlib/vector")
               (:file "./CL/stdlib/string")
               (:file "./CL/functions"
                      :depends-on ("./CL/macros"))
               ;; (:file "threadstest"
               ;;        :depends-on ("./CL/functions"))
               (:file "./CL/stdlib/ref")
               (:file "./CL/stdlib/lazy")
               (:file "./CL/stdlib/lazy-cons"
                      :depends-on ("./CL/stdlib/lazy"))
               (:file "./CL/stdlib/struct"
                      :depends-on ("./CL/stdlib/lazy-cons"))
               (:file "./CL/stdlib/tuple")
               (:file "./data-structures/functor-applicative-monad")
               (:file "./data-structures/state"
                      :depends-on ("./CL/stdlib/tuple"
                                   "./data-structures/functor-applicative-monad"))
               (:file "./data-structures/reader"
                      :depends-on ("./data-structures/functor-applicative-monad"))))
