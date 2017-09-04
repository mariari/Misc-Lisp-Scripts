(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload 'inferior-shell)
  (ql:quickload "optima"))

(defpackage #:shell
  (:nicknames #:fun :times)
  (:use #:optima)
  (:use #:inferior-shell)
  (:use #:common-lisp))

(in-package :shell)

(run/ss `(pipe (echo (+ hel "lo,") world) (tr "hw" "HW") (sed -e "s/$/!/")))

(run/ss `(echo stuff is here ,(+ 2 3 45)))

(run/ss `(pipe (ls "~/") (grep "he")))
