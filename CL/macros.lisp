; (load "~/Documents/Workspace/Lisp/CommonLisp/functions.lisp")

(ql:quickload '(:fare-quasiquote-readtable
                :trivia
                ;; :let-over-lambda
                ))

(defpackage #:macros
  (:documentation  "Random assortment of macros that make my life easier")
  (:use #:let-over-lambda)
  (:shadowing-import-from #:let-over-lambda #:when-match #:if-match)
  (:use #:trivia
        #:sb-ext
        #:common-lisp)
  (:export #:once-only
           #:let1
           #:sh-run-stream))

(in-package :macros)

(defmacro once-only ((&rest names) &body body)
  " USAGE: creates a gensym for the arguments given to the variables of the function,
    uses them only once, and avoids capture (once-only (arg1 arg2))"
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(mapcar (lambda (g) `(,g (gensym))) gensyms))
       `(let (,,@(mapcar (lambda (n g) ``(,,g ,,n)) names gensyms))
          ,(let (,@(mapcar (lambda (n g) `(,n ,g)) names gensyms))
                ,@body)))))

;; from Land of Lisp
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defmacro sh-run-stream (output command arguments &body body)
  `(let1 ,output (make-string-output-stream)
     (run-program ,(if (directory command)
                       command
                       (concatenate 'string "/bin/" command))
                  ,arguments  :output ,output)
     ,@body))

;; Unused for history -------------------------------------------------------------------------
(defmacro once-only% ((&rest names) &body body)
  " USAGE: creates a gensym for the arguments given to the variables of the function,
    uses them only once, and avoids capture (once-only (arg1 arg2))"
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))
