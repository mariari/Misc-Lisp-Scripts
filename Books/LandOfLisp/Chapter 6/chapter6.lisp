(defun say-hello ()
  (princ "Who is the user?")
  (princ #\newline)
  (let ((name (read-line)))
    (princ "Nice to meet you, ")
    (princ name)))

(defparameter *foo* '(+ 1 2))

(eval *foo*)


