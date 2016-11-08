(ql:quickload "optima")
(ql:quickload "inferior-shell")
(asdf:load-system :uiop)
(load "~/Documents/Workspace/Lisp/CommonLisp/functions.lisp")

(defpackage #:shell
  (:nicknames #:fun :times)
  (:use #:optima)
  (:use #:inferior-shell)
  (:use #:uiop)
  (:use #:common-lisp))

(in-package :shell)


(run/ss `(pipe (echo (+ hel "lo,") world) (tr "hw" "HW") (sed -e "s/$/!/")))

(run/ss `(pipe (ls) (grep "he")))
;; (run/ss `(grep ))

;; (run/ss `(pipe (echo ,(lss "~/")) (grep "he")))

(time (split-by-char (run/ss `(pipe (echo ,(lss "~/")) (grep ".") (sed -e "s/o/0/g"))) #\linefeed))
(time (split-by-char (run/ss `(pipe (ls /home/loli/) (grep ".") (sed -e "s/o/0/g"))) #\linefeed))

(let ((stream (make-string-output-stream)))
  (run-program "ls ~" :output stream)
  (get-output-stream-string stream))

;; (uiop:detect-os)
;; (uiop:read-little-endian)
