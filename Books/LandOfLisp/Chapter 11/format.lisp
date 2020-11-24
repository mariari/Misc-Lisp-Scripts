;; Money~
(format t "Add onion rings for only ~$ dollars more!" 1.5)

;; Messing around 
(princ (reverse (format nil "Add onion rings for only ~$ dollars more!" 1.5)))
; ~10 will make the first argument be 10 char... so 7 spaces 
(format t "I am printing ~10a within ten spaces of room." "foo")
; Same as before but the @ puts the spaces first 
(format t "I am printing ~10@a within ten spaces of room." "foo")
; Each space is followed by 2... so it does it in sets of 3 
(format t "I am printing ~10,3a within ten (or more) spaces of room." "foo")
;; This makes it so that there will be 4 spaces after the word (,, = blank parameters)
(format t "I am printing ~,,4a in the middle of this sentence." "foo")
;; Using things besides spaces!!!!
(format t "The word ~,,4,'!@a feels very important." "foo")

;; Formatting integers------------------------------------------

;; Hexa 
(format t "the number 1000 in hexadecimal is ~x" 1000)

;; Binary
(format t "The number 1000 in binary is ~b" 1000)

;; Decimal
(format t "The number 1000 in decimal is ~d" 1000)
; Commas!!!
(format t "Numbers with commas in them are ~:d times better." 1000000)

;; Formating floating point-------------------------------------

; ~4f only displays 4 characters 
(format t "PI can be estimated as ~4f" 3.141593)

; the second slot dictates how many numbers after the . there are 
(format t "PI can be estimated as ~,4f" 3.141593)

; The third slot dictates to the power of 10 the number should be multiplied by 
(format t "Percentages are ~,,2f percent better than fractions" 0.77)

;; Multiple Lines-----------------------------------------------
(progn (princ 22)
       ;; Like fresh-line except it always makes a new line 
       (terpri)
       (princ 33))

;; We get this functionality with format t with ~% = Teri and ~& = fresh-line
(progn (format t "this is on one line ~%")
       (format t "~%this is on another line"))

(progn (format t "this is on one line ~&")
       (format t "~&this is on another line"))

;; Justifying output -------------------------------------------

(defun random-animal ()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))

(loop repeat 10
   ;; Displays the first a on the 5th character pos, 2nd a at the 15th and the 3rd a on the 25th 
   do (format t "~5t~a ~15t~a ~25t~a~%"
	      (random-animal)
	      (random-animal)
	      (random-animal)))

;; Say we just want it 30 characters wide... Though our first attempt fails 
(loop repeat 10
   ;; ~; just means next variable 
   do (format t "~30<~a~;~a~;~a~>~%"
	      (random-animal)
	      (random-animal)
	      (random-animal)))

;; The @ symbol makes sure that they are aligned... which means we can improve the above statement 
(loop repeat 10 do (format t "~30:@<~a~>~%" (random-animal)))

;; This is a bit better... buts its not perfect
(loop repeat 10
   do (format t "~30:@<~a~;~a~;~a~>~%"
	      (random-animal)
	      (random-animal)
	      (random-animal)))

;; There we go. we separated each statement into 3 separate 10 length lines 
(loop repeat 10
   do (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
	      (random-animal)
	      (random-animal)
	      (random-animal)))

;; WTF CONTROL SEQUENCES!?!?!?!?!-------------------------------

(defparameter *animals* (loop repeat 10 collect (random-animal)))

; ~{ ~} loops! 
(format t "~{I see a ~a! ~}" *animals*)

;; We can even grab 2 at once
(format t "~{I see a ~a... or was it a ~a?~%~}" *animals*)

;; JUST WTF 
(format t "|~{~<|~%|~,33:;~2d ~>~}|" (loop for x below 100 collect x))
