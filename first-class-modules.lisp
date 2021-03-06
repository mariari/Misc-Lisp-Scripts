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
  (reference:ref start-at))

(defun eval-t (state sexp)
  (let ((val (reference:! state)))
    (if (null sexp)
        (list :ok (prog1 val (setf (reference:! state) (1+ val))))
        (list :error "sexp is not empty"))))

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

(defparameter *test-dir* (list-dir:create "~/"))

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

(defun build-dispatch-tables (handlers)
  (let ((table (make-hash-table :test 'equalp)))
    (mapc (lambda (instance)
            (setf (gethash (val-s (val-s instance 'query-handler) 'name)
                           table)
                  instance))
          handlers)
    table))

(defun dispatch (dispatch-table name-and-query)
  (if (and (not (null name-and-query)) (listp name-and-query))
      (let* ((name   (car name-and-query))
             (query  (cadr name-and-query))
             (module (gethash name dispatch-table)))
        (if module
            (call-s (val-s module 'query-handler) 'eval-t
                    (val-s module 'this)
                    query)
            (list :error "Could not find matching handler")))
      (list :error "malformed query")))

;; interestingly enough gensym breaks at top level
;; (defparameter *unique-instance*   (build-instance *unique* 0))
;; (defparameter *list-dir-instance* (build-instance *list-dir* "~/"))
;; (defparameter *table* (build-dispatch-tables (list *unique-instance* *list-dir-instance*)))
;; (dispatch *table* (list "ls" "./"))
;; (dispatch *table* (list "unique" '()))
