(defpackage #:struct
  (:documentation "Provides functions for struct definitions")
  (:use #:common-lisp)
  (:export defstruct-l))

(in-package :struct)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:let-over-lambda :trivia)))

;; EXTRA DEPENDENCEIS :: we don't require lazy, but this is made
;; explicitly for lazy-cons

;; so far this macro only supports the :type keyword
;; this macro does not support conc-n yet, but at a later date I'll add it
;; it supports options in name and options, but it does not make lazy version of inner
(defmacro defstruct-l (name-and-options &rest slot-descriptions)
  "works just like defstruct, except that it allows for the auto-generation of a lazy constructor
   and getters that conform to automatic lazy expansion, if none of these features are used
   then it's just defstruct"
  (let ((name (if (listp name-and-options) (car name-and-options) name-and-options)))
    (labels ((turn-into-accessor   (symb) (concat-symb (lol:mkstr name) "-" (lol:mkstr symb)))
             (turn-into-accessor-l (symb) (concat-symb (lol:mkstr (turn-into-accessor symb)) "-L"))
             (turn-into-key-word   (symb) (intern (lol:mkstr symb) "KEYWORD")))

      (let* ((doc-string        (car (remove-if-not #'stringp slot-descriptions)))
             (slot-descriptions (remove-if #'stringp slot-descriptions))
             (new-body          (mapcar (lambda (slot)
                                          (if (atom slot)
                                              slot
                                              (mapcan (lambda (x) ; inject the lazy type to make consistent types
                                                        (if (eq (car x) :type)
                                                            `(:type (or lazy:lazy ,(cadr x)))
                                                            x))
                                                      (lol:group slot 2))))
                                        slot-descriptions))
             (new-symbs-default (mapcar (lambda (slot)
                                          (if (listp slot)
                                              (list (car slot) (cadr slot))
                                              slot))
                                        new-body))
             (new-symbs         (mapcar (lambda (slot) (if (listp slot) (car slot) slot)) new-body))
             (new-symbs-make    (mapcar #'turn-into-accessor   new-symbs))
             (new-symbs-make-l  (mapcar #'turn-into-accessor-l new-symbs))
             (struct-creator    (concat-symb "MAKE-" (lol:mkstr name)))
             (struct-creator-l  (concat-symb (lol:mkstr struct-creator) "-L"))
             (value             (gensym)))
        `(prog1
             ,(if doc-string
                  `(defstruct ,name-and-options ,doc-string ,@new-body)
                  `(defstruct ,name-and-options ,@new-body))
           ,@(mapcar (lambda (maker-symb-l maker-symb)
                       `(defun ,maker-symb-l (,name)
                          (let ((,value (,maker-symb ,name)))
                            (if (lazy:lazy-p ,value)
                                (setf (,maker-symb ,name) (lazy:force ,value))
                                ,value))))
                     new-symbs-make-l new-symbs-make)
           (defmacro ,struct-creator-l (&key ,@new-symbs-default)
             "creates a lazy version of the struct, delaying all the arguments"
             ;; (list '...) because we are doing ` expansion by hand
             ;; as we need to compile into a defmacro form
             ;; and double ` would add extra , which is uneeded
             (list ',struct-creator ,@(mapcar (lambda (x)
                                                (if (keywordp x)
                                                    x
                                                    `(list 'lazy:delay ,x))) ; make the elements delayed
                                              (lol:flatten (mapcar (lambda (symb)
                                                                     (list (turn-into-key-word symb) symb))
                                                                   new-symbs))))))))))
(defun concat-s (&rest body)
  (apply #'concatenate 'string body))

(defun concat-symb (&rest body)
  (lol:symb (apply #'concat-s body)))

;;;; old idea that I would like the answer to sometime
;; FIX ME, for some reason I can't overset the default getters and setters
;; I tired to ways of solving the problem below

;; the first uses setf and not inling the functions to get the effect
;; this approach has the issue of using the old definition for some reason
;; it's bizarre

;; the second makes a new function of the same name and unbinds the original
;; this second method gets stuck in infinite recursion of calling itself over and over

;; 1.
;; ,@(mapcar (lambda (maker-symb)
;;             `(eval-when (:compile-toplevel :load-toplevel :execute)
;;                (setf (symbol-function ',maker-symb)
;;                      (lambda (,name-and-options)
;;                        (let ((,value (,maker-symb ,name-and-options)))
;;                          (if (lazy-p ,value)
;;                              (setf (,maker-symb ,name-and-options) (force ,value))
;;                              ,value))))))
;;           new-symbs-make)

;; 2.
;; ,@(mapcar (lambda (maker-symb)
;;             `(flet ((,maker-symb (,name-and-options)
;;                       (let ((,func #',maker-symb))
;;                         (let ((,value (funcall ,func ,name-and-options)))
;;                           (if (lazy-p ,value)
;;                               (setf (,maker-symb ,name-and-options) (force ,value))
;;                               ,value)))))
;;                (fmakunbound ',maker-symb)
;;                (defun ,maker-symb (,name-and-options)
;;                  (funcall ,maker-symb ,name-and-options))
;;                ;; (declaim (inline ,maker-symb))
;;                ))
;;           new-symbs-make)

;; for the mean time use this definition instead and call -l when the function is lazy, or ∀ calls in general


;;;; Some examples to work if you wish to see the expansion

;; (defconstant +empty-t+ :empty
;;   "Used for the empty type of finger tree")

;; (defstruct-l single
;;     "single will be of type FingerTree"
;;   ele)

;; (deftype finger-tree ()
;;   "the data structure finger tree is composed of empty, single, and deep"
;;   `(or (eql :empty)
;;       (satisfies single-p)
;;       (satisfies deep-p)))


;; (defstruct-l deep
;;     "this is the spine of the finger tree, along with some digits on the sides"
;;   (left (make-digit) :type digit) ; the default values are a hack and should thus never be used!
;;   (spine :empty :type finger-tree)
;;   (right (make-digit) :type digit))


;; (defstruct-l digit
;;     "a digit can hold either 1 or 2 or 3 or 4 things in it"
;;   one two three four)

;; (defstruct-l node
;;     "a node either has two or three things in it"
;;   one two three)

;; (defstruct-l view
;;     "a view of a finger-tree, gives back an element and the rest of the tree"
;;   (ele nil)
;;   (tree :empty :type (or finger-tree lazy)))
