(defpackage #:package
  (:documentation "Provides functions for packages")
  (:use #:common-lisp)
  (:export #:export-all))

(in-package :package)

(defun export-all (&optional (package-name *package*))
  (let ((package (find-package package-name)))
    (do-symbols (sym package)
      (when (and (eql (symbol-package sym) package)
                 (or (boundp package)
                     (fboundp package)))
        (export sym)))))

;; taken from
;; http://reference-error.org/2015/08/30/common-lisp-finding-all-functions-in-a-package.html
(defun all-function-symbols (package-name)
  "Retrieves all function symbols from a package."
  (declare ((or package string symbol) package-name))
  (the list
       (let ((lst (list))
             (package (find-package package-name)))
         (cond (package
                (do-all-symbols (symb package)
                  (when (and (fboundp symb)
                             (eql (symbol-package symb) package))
                    (push symb lst)))
                lst)
               (t
                (error "~S does not designate a package" package-name))))))
