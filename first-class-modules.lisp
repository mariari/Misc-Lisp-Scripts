
(defmacro make-query-module (name)
  `(defpackage ,name
     (:use #:cl)
     (:export :name
              :create
              :eval-t)))

(defparameter *unique*
  (make-query-module #:unique))

(in-package unique)

(defparameter name "unique")

(defun create (start-at)
  start-at)

(defun eval-t (state sexp)
  (if (null sexp)
      (list :ok (prog1 state (incf state)))
      (list :error "sexp is not empty")))

(in-package cl-user)

(defparameter *list-dir*
  (make-query-module #:list-dir))

(in-package list-dir)

(defparameter name "ls")

(defstruct cwd val)

(defun create (cwd)
  (make-cwd :val cwd))

(defun eval-t (state sexp)
  (if (stringp sexp)
      (let ((dir (if (eq :absolute (car (pathname-directory sexp)))
                     sexp
                     (concatenate 'string (cwd-val state) sexp "/*.*"))))
        (list :ok (directory dir)))
      (list :error "Sexp is not a string")))

(in-package cl-user)

(defparameter *test-dir* (list-dir:create "/home/loli/"))


;; In OCaml we need a nested module, in order to make this work, we will follow suit, but a struct would work

(defmacro build-instance (module config &optional (gensym-name "G"))
  (let* ((gensym (gensym gensym-name))
         (package (make-package gensym :use '(#:cl))))
    `(progn
       (export (defparameter ,(intern (symbol-name 'q) package) ,module)
               ,package)

       (export (defparameter ,(intern (symbol-name 'this) package)
                 (funcall (find-symbol (symbol-name 'create) ,module) ,config))
               ,package)
       ,package)))

(defparameter *unique-instance*   (build-instance *unique* 0 "GE"))
(defparameter *list-dir-instance* (build-instance *list-dir* "/home/loli/" "LIS"))


