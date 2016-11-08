(defparameter *holder* (make-string-output-stream))

(princ "Blah blah blah, I'm smart, and yummier dumb" *holder*)

(get-output-stream-string *holder*)
