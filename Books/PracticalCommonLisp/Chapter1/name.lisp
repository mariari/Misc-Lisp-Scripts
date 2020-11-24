(print "what's your name")

(defvar *name* (read))

(defun hello-you (name)
  (format t "Hello ~a! ~%" name))

(setq *print-case* :capitalize)  ;; :upcase :downcase 

(hello-you *name*)
