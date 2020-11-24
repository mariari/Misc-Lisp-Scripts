(ql:quickload "usocket")


(write-char #\x *standard-output*)

(input-stream-p *standard-input*)

(read-char *standard-input*)

(print "foo" *standard-output*)

;; File manipulation--------------------------------------------

(with-open-file (my-stream  "data.txt" :direction :output :if-exists :supersede)
  (print "my datas" my-stream))

(with-open-file (my-stream "data.txt" :direction :input )
  (read my-stream))



(print *compile-file-truename*)

(run-program "/bin/ls" (list "./")
	     :output *standard-output*)
;; String builder like functions--------------------------------
(with-output-to-string (*standard-output*)
  (princ "the sum of ")
  (princ 5)
  (princ " and ")
  (princ 2)
  (princ " is ")
  (princ (+ 2 5)))
