(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:fare-quasiquote-readtable
                  :trivia
                  :let-over-lambda)))

(defpackage #:functions
  (:nicknames #:fn #:f)
  (:documentation "Random assortment of functions that make my life easier")
  (:use #:let-over-lambda)
  (:shadowing-import-from #:let-over-lambda #:when-match #:if-match)
  (:use #:sb-ext
        #:common-lisp
        #:macros
        #:trivia)
  (:export :ls
           :lsl
           :lss
           :fact
           :nrotate-left
           :my-command-line
           :split-on
           :replace-all
           :compose
           :curry
           :curryf))

(in-package :functions)

(defun ls (&optional &rest dir)
  "acts like ls and puts all the elemnts into a list"
  (split-by-delim (apply #'lss dir)
                 #\linefeed))

(defun lsl (&optional &rest dir)
  "puts lss into a 2 layered list"
  (cond ((cdr dir) (mapcar #'ls dir))
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


(defun fact (n &optional (bot 1))
  (nlet-tail fact ((n n) (acc 1))
    (if (= (- bot 1) n)
        acc
        (fact (- n 1) (* n acc)))))

(defmacro curry (fn . args)
  "Creates a partially applied function that takes 1 argument if it is a macro
   (a limitation of &rest closures in CL) and multiple if it is a function"
  (if (functionp (macro-function fn))
      `(currym ,fn ,@args)
      `(curryf #',fn ,@args)))

;; Maybe use macrolet to create our lexical closure or at least get the list so we can take multiple arguments
(defmacro currym (fn . args)
  "Creates a partially applied function that takes 1 argument"
  (let ((arg (gensym)))
    `(lambda (,arg) (,fn ,@args ,arg))))

(declaim (ftype (function (function &rest t) function) curryf)
         (inline curryf))
(defun curryf (fn &rest args)
  "Creates a partially applied function that takes many argument"
  (lambda (&rest args2) (apply fn (append args args2))))


(defun compose (&rest fns)
  "Very much like a normal lisp compose, except that it only applies the first args
   to the far right function instead of all args, the rest of the args
   gets sent to the first function in the list (car position)"
  (if fns
      (let* ((fns (reverse fns)))
        (alambda (&rest args)
          (if args                        ; checks if any arguments were given
              (funcall (alambda (fns arg) ; do the function if args are given
                         (match fns
                           ((list f)     (apply f (cons arg (cdr args))))
                           ((list* f fs) (self fs (funcall f arg)))))
                       fns (car args))
              (curry self))))             ; else just wait for proper inputs
      #'identity))

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
