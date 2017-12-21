(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :trivia)
  (use-package 'trivia))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (SETF *ARITY-CHECK-BY-TEST-CALL* NIL)

  (defstruct monoid-set
    (mempty mempty :type function)
    (<>     <>     :type function)
    (bar    bar    :type function))

  (defstruct custom-finger
    (tree   :empty :type finger-tree)
    (monoid (make-monoid-set) :type monoid-set)))

(defparameter *default-monoid* (make-monoid-set))


;; the body is going to be a lambda, so let the user pass it!
(defun defmonoid (monoid-name type fn)
  (lambda (&rest args)
    (if (or (not (valid-type-specifier-p type)) (typep (car args) type))
        (apply fn args)
        (apply monoid-name args))))


(defmacro treefun (name args tree &body body)
  (let ((monoid (gensym "Monoid")))
    `(defun ,name ,args
       (let* ((,monoid (custom-finger-monoid ,tree))
              (mempty  (monoid-set-mempty ,monoid))
              (<>      (monoid-set-<> ,monoid))
              (bar     (monoid-set-bar ,monoid)))
         ,@body))))

(defmacro treefmacro (name args tree &body body)
  (let ((monoid (gensym "Monoid")))
    `(defmacro ,name ,args
       (let* ((,monoid (custom-finger-monoid ,tree))
              (mempty  (monoid-set-mempty ,monoid))
              (<>      (monoid-set-<> ,monoid))
              (bar     (monoid-set-bar ,monoid)))
         ,@body))))

(treefun c-cons-l (x c-tree) c-tree
  (cons-l x (custom-finger-tree c-tree)))

(defun create-monoid-set (type &key mempty-f bar-f <>-f (monoid *default-monoid*))
  (let ((d-mempty (monoid-set-mempty monoid))
        (d-bar    (monoid-set-bar    monoid))
        (d-<>     (monoid-set-<>     monoid)))
    (make-monoid-set :mempty (if mempty-f (defmonoid d-mempty type mempty-f) d-mempty)
                     :bar    (if bar-f    (defmonoid d-bar    type bar-f)    d-bar)
                     :<>     (if <>-f     (defmonoid d-<>     type <>-f)     d-<>))))

(treefun deep-measure-d (c-tree) c-tree
  (deep-measure-l (custom-finger-tree c-tree)))

(defparameter *test*
  (create-monoid-set 'number
                     :mempty-f (constantly 1)
                     :<>-f     #'*))


(defparameter *test-tree*
  (make-custom-finger :tree   (cons-l 1 (cons-l 1 (cons-l 4 (cons-l 4 :empty))))
                      :monoid *test*))
