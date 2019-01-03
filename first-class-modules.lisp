;;; General functions---------------------------------------------------------------------------------

(defun call-s (module name &rest args)
  "Finds a function-symbol in a namespace and calls it on args"
  (apply #'funcall
         (find-symbol (symbol-name name) module)
         args))

(defun val-s (module name)
  "Finds a symbol-value in a module"
  (symbol-value (find-symbol (symbol-name name) module)))

;;;; Query Handler-----------------------------------------------------------------------------------

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
       (export (defparameter ,(intern (symbol-name 'query-handler) package) ,module)
               ,package)

       (export (defparameter ,(intern (symbol-name 'this) package)
                 (call-s ,module 'create ,config))
               ,package)
       ,package)))

(defparameter *unique-instance*   (build-instance *unique* 0))
(defparameter *list-dir-instance* (build-instance *list-dir* "~/"))

(defun build-dispatch-tables (handlers)
  (let ((table (make-hash-table :test 'equalp)))
    (mapc (lambda (instance)
            (setf (gethash (val-s (val-s instance 'query-handler) 'name)
                           table)
                  instance))
          handlers)
    table))
