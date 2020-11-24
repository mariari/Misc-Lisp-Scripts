;; CLISP makes them nil nil nil.... sbcl makes it 0 0 0
(make-array 3 :initial-element nil)

(defparameter x (make-array 3))
;; Used to grab the first element 
(aref x 1)
;; Used to set the first element 
(setf (aref x 1) 'foo)
(setf foo '(a b c))

(second foo)
(setf (second foo) 'z)
;; Like aref for arrays 
(nth 2 foo)
;; Hashes-------------------------------------------------------
(defparameter x (make-hash-table))
;; gethash is basically getting the has's key 
(setf (gethash 'yup x) '25)

;; Multiple-returns---------------------------------------------
;; Returns 3 and then 7 
(defun boo ()
  (values 3 7))

;; takes the 3 
(+ (boo) 3)

;; we Can use the 7 though
(multiple-value-bind (a b) (boo)
  (* a b))

;; Structures OOP fuck yeah nigga ------------------------------
(defstruct person
  name
  age
  waist-size
  favorite-color)

(defparameter *bob* (make-person :name "Bob"
				 :age 35
				 :waist-size 32
				 :favorite-color "blue"))

(person-age *bob*)

(setf (person-age *bob*) 36)

;; Can just pass it the function as well
(defparameter *that-guy* #S(PERSON :NAME "Bob" :AGE 35 :WAIST-SIZE 32 :FAVORITE-COLOR "blue"))

(reduce #'+ '(3 4 5 6 7))

(apply #'+ '(3 4 5 6 7))

(reduce (lambda (best item)
	  (if (and (evenp item) (> item best))
	      item
	      best))
	'(7 4 6 5 2)
	:initial-value 0)
;; Methods... Reminds me of Java--------------------------------
(defmethod add ((a number) (b number))
  (+ a b))

(defmethod add ((a list) (b list))
  (append a b))

(add '(a b) '(c d))

(add 3 4)

;; Getting path names
(merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))

;; Tie this with load since it will be nil if the file is not found... can do (when probe-file (load probe-file))
(probe-file (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
