(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:fare-quasiquote-readtable
                  :trivia
                  :let-over-lambda)))

(defpackage #:functions
  (:nicknames #:f)
  (:documentation "Random assortment of functions that make my life easier")
  (:use #:let-over-lambda)
  (:shadowing-import-from #:let-over-lambda #:when-match #:if-match)
  (:use #:sb-ext
        #:common-lisp
        #:macros
        #:trivia)
  (:export :ls :lsl
           :lss :range :fact :range-v :nrotate-left
           :my-command-line :split-by-delim
           :ncircular :circular
           :split-on :replace-all))

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

(defun range (first &optional (second nil) (step 1))
  (macrolet ((for (second word first)
               `(loop :for x :from ,second ,word ,first :by step
                   collect x)))
    (cond ((and second (> second first)) (for first to     second))
          (second                        (for first downto second))
          (t                             (for 0     to     first)))))
;; (declaim (inline range))

;; we abs second - first as we want floor to always go down, not up
(declaim (ftype (function (fixnum &optional (or fixnum null) fixnum) (simple-array fixnum (*))) range-v))
(defun range-v (first &optional (second) (step 1))
  "returns a range in a vector, much faster than range, but only supports fixnums"
  (flet ((compute (first second)
           (let ((vec      (make-array (1+ (floor (abs (- second first)) step)) :element-type 'fixnum))
                 (new-step (if (> first second) (- step) step))) ; we will go down if first > second else up
             (dotimes (i (length vec) vec)
               (setf (aref vec i) (+ first (the fixnum (* new-step i))))))))
    (declare (inline compute))
    (if second
        (compute first second)
        (compute 0     first))))

(declaim (inline range-v))
;; (time (defparameter *x* (range-v 0 100000 1)))


;; (print (range-v 10))

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

;; From Lisp
(defun nrotate-left (list &aux (last (last list)))
  "Rotate elements of list to the left, circularly."
  (declare (type list list))
  (if list
      (setf (cdr last) list
            list (cddr last)
            (cddr last) nil))
  list)


(defun split-on (delim seq)
  (let ((length (length delim)))
    (labels ((rec (seq cps)
               (let ((num (search delim seq)))
                 (if num
                     (rec (subseq seq (+ length num))
                          (lambda (x)
                            (funcall cps (list* (subseq seq 0 num) (subseq seq num (+ length num)) x))))
                     (funcall cps (list seq))))))
      (rec seq #'identity))))

(defun replace-all (old new seq)
  (mapcar (lambda (x num)
            (if (zerop num) x new))
          (split-on old seq) '#1=(0 1 . #1#)))

(defun circular (list)
  (when list
    (let ((new-list (copy-list list)))
      (setf (cdr (last new-list)) new-list))))

(defun ncircular (list)
  (when list
    (setf (cdr (last list)) list)))
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


;; this version is a lot slower than range-v
(defun range-v% (first &optional (second 0) (step 1))
  "returns a vector range, which is slightly faster than range"
  (declare (type fixnum step))
  (let* ((length   (1+ (1+ (floor (abs (- second first)) step))))
         (vec      (make-array length :element-type 'Integer))
         (new-step (if (< second first)
                         (- step)
                         step))
         (current  (- first new-step)))
    (declare (type fixnum new-step))
    (dotimes (i (length vec) vec)
      (setf (aref vec i) (incf current new-step)))))

(defun replace-all% (old new seq)
  (cadr (reduce (lambda (x acc)
                  (list (1+ (car acc))
                        (if (evenp (car acc))
                            (cons x (cadr acc))
                            (cons new (cadr acc)))))
                (split-on old seq)
                :initial-value (list 0 '()) :from-end t)))