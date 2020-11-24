(load "chapter3.lisp")

(defun |#"-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let (chars)
    (do ((prev (read-char stream) curr)
         (curr (read-char stream) (read-char stream)))
        ((and (char= prev #\#) (char= curr #\")))
      (push prev chars))
    (coerce (nreverse chars) 'string)))

(set-dispatch-macro-character #\# #\" #' |#"-reader|)

;; #"contains " and \."#

(defun segment-reader (stream ch n)
  (if (> n 0)
      (let ((chars))
        (do ((curr (read-char stream)
                   (read-char stream)))
            ((char= ch curr))
          (push curr chars))
        (cons (coerce (nreverse chars) 'string)
              (segment-reader stream ch (- n 1))))))

(ql:quickload "cl-ppcre")

#+cl-ppcre
(defmacro! match-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:scan
       ,(car ,g!args)
       ,',g!str)))

#+cl-ppcre
(defmacro! subst-mode-ppcre-lambda-form (o!args)
  ``(lambda (,',g!str)
      (cl-ppcre:regex-replace-all
       ,(car ,g!args)
       ,',g!str
       ,(cadr ,g!args))))

(time (eval (match-mode-ppcre-lambda-form '(#\c #\b 3 4 5 6 6))))
(funcall (eval (match-mode-ppcre-lambda-form '(#\c #\b 2 #\3)) ) "#\c #\b 2 #\3")


#+cl-ppcre
(defun |#~-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((mode-char (read-char stream)))
    (cond
      ((char= mode-char #\m)
       (match-mode-ppcre-lambda-form
        (segment-reader stream
                        (read-char stream)
                        1)))
      ((char= mode-char #\s)
       (subst-mode-ppcre-lambda-form
        (segment-reader stream
                        (read-char stream)
                        2)))
      (t (error "Unkown #~~ mode character")))))

#+cl-ppcre
(set-dispatch-macro-character #\# #\~ #'|#~-reader|)

;; #~m/abc/
(#~m/abc/ "abc123")
(#~s/abc/def/ "abc123")

(equalp (match-mode-ppcre-lambda-form  (list "abc")) '#~m/abc/)
(+ 1 2 3)


(defvar not-shared '((1) (1)))

(eq (car not-shared) (cadr not-shared))



(defvar shared '(#1=(1) #1#))

(eq (car shared) (cadr shared))

(list #1=(list 0)
      #1#
      #1#)

(defparameter x 0)

(list #1=(incf x) #1# #1#)


(defun cyclic-p (l)
  (cyclic-p-aux l (make-hash-table)))

(defun cyclic-p-aux (l seen)
  (if (consp l)
      (or (gethash l seen)
         (progn
           (setf (gethash 1 seen) t)
           (or (cylic-p-aux (car l) seen)
              (cylic-p-aux (cdr l) seen))))))
(let ((x '(hello hello hello)))
  (type-of x))


(defun iterate-forever
    (print '#1=(hello . #1#)))

;; Security checks

(defvar safe-read-from-string-blacklist
  `(#\# #\: #\|))

(let ((rt (copy-readtable nil)))
  (defun safe-reader-error (stream closech)
    (declare (ignore stream closech))
    (error "safe-read-from string failure"))
  
  (dolist (c safe-read-from-string-blacklist)
    (set-macro-character
     c #'safe-reader-error nil rt))
  
  (defun safe-read-from-string (s &optional fail)
    (if (stringp s)
        (let ((*readtable* rt) *read-eval*)
          (handler-bind
              ((error (lambda (condition)
                        (declare (ignore condition))
                        (return-from safe-read-from-string fail))))
            (read-from-string s)))
        fail)))


(let ((let '`(let ((let ',let))
               ,let)))
  `(let ((let ',let)) ,let))



(defun range (minn &optional (maxx nil) (stepp 1))
  (labels ((range (minn maxx stepp fun)
             (if (= minn maxx)
                 (list maxx)
                 (cons minn (range (funcall fun minn stepp) maxx stepp fun)))))
    (cond
      ((null maxx)      (range 0 minn stepp #'+))
      ((> minn maxx) (range minn maxx stepp #'-))
      (t             (range minn maxx stepp #'+)))))

