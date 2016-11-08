(ql:quickload '(:fare-quasiquote-readtable
                :trivia
                :let-over-lambda))

(load "~/Documents/Workspace/Lisp/CommonLisp/macros.lisp")

(defpackage #:functions
  (:documentation "Random assortment of functions that make my life easier")
  (:use #:let-over-lambda)
  (:shadowing-import-from #:let-over-lambda #:when-match #:if-match)
  (:use #:sb-ext
        #:macros
        #:common-lisp
        #:trivia)
  (:export :ls :lsl
           :lss :range :fact
           :my-command-line :split-by-delim))

(in-package :functions)

(defun ls (&optional &rest dir)
  "acts like ls and puts all the elemnts into a list"
  (split-by-delim (apply #'lss dir)
                 #\linefeed))

(defun lsl (&optional &rest dir)
  "puts lss into a 2 layered list"
  (cond ((cdr dir) (mapcar (lambda (x) (ls x)) dir))
        (dir       (list (apply #'ls dir)))
        (t         (list (ls)))))

(defun lss (&optional &rest dir)
  "Acts like the normal ls command on Linux."
  (sh-run-stream stream "ls"
      (if dir
          (mapcar (lambda (i)
                    (directory-namestring (car (directory i))))
                  dir)
          (list "./"))
    (get-output-stream-string stream)))


(defun range (max &optional (min 0) (step 1))
  (loop for x from min to max by step
     collect x))

(declaim (inline range))

(defun fact (n &optional (bot 1))
  (nlet-tail fact ((n n) (acc 1))
    (if (= (- bot 1) n)
        acc
        (fact (- n 1) (* n acc)))))

;; taken from http://cl-cookbook.sourceforge.net/os.html#accessing-command-line
(defun my-command-line ()
  (or 
   #+SBCL *posix-argv*  
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

;;; Helper functions---------------------------------------------------------------------------
(defun split-by-delim (delim seq)
    "Returns a list of substrings of seq"
    (loop for i = 0 then (1+ j)
          as  j = (position delim seq :start i)
          collect (subseq seq i j)
       while j))

(defun split-by-delim! (seq delim)
  (labels ((tco (&optional (acc '()) (i 0) (j (position delim seq :start i)))
             (if (null j)
                 (cons (subseq seq i) acc)
                 (tco (cons (subseq seq i j) acc) (1+ j)))))
    (reverse (tco))))


;; Lost with the time--------------------------------------------------------------------------
(defun fact% (num &optional (bot 1))
  (reduce #'* (range num bot)))

(defparameter image nil)
(progn
  (setf image (coerce (loop for i = (round (random (expt 2 24))) repeat 1000 collecting i) 'vector))
  nil)
