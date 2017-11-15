(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:let-over-lambda :trivia)))

;; EXTRA DEPENDENCEIS :: we don't require lazy, but this is made
;; explicitly for lazy-cons


;; so far this macro only supports the :type keyword
;; this macro does not support conc-n yet, but at a later date I'll add it
;; for some reason undefining 
(defmacro defstruct-l (name-and-options &rest slot-descriptions)
  "works just like defstruct, except that it allows for the auto-generation of a lazy constructor
   and getters that conform to automatic lazy expansion, if none of these features are used
   then it's just defstruct"
  (let ((name (if (listp name-and-options) (car name-and-options) name-and-options)))
    (flet ((turn-into-accessor (symb) (lol:symb (concatenate 'string
                                                             (lol:mkstr name)
                                                             "-" (lol:mkstr symb))))
           (turn-into-accessor-l (symb) (lol:symb (concatenate 'string
                                                               (lol:mkstr name)
                                                               "-" (lol:mkstr symb) "-L")))
           (turn-into-key-word (symb) (intern (concatenate 'string (lol:mkstr symb)) "KEYWORD")))

      (let* ((doc-string (car (remove-if-not #'stringp slot-descriptions)))
             (slot-descriptions (remove-if #'stringp slot-descriptions))
             (new-body
              (mapcar (lambda (slot)
                        (if (not (listp slot))
                            slot
                            (let ((seen-type nil)) ; we just want to inject the lazy type to make consistent types
                              (mapcar (lambda (x) ; or else the user given types would complain
                                        (cond (seen-type       (setf seen-type nil x `(or lazy ,x)))
                                              ((equal :type x) (setf seen-type t) :type)
                                              (t                x)))
                                      slot))))
                      slot-descriptions))
             (new-symbs-with-default (mapcar (lambda (slot)
                                               (if (listp slot)
                                                   (list (car slot) (cadr slot))
                                                   slot))
                                             new-body))
             (new-symbs      (mapcar (lambda (slot)                     (if (listp slot) (car slot) slot)) new-body))
             (new-symbs-make (mapcar (lambda (slot) (turn-into-accessor (if (listp slot) (car slot) slot))) new-body))
             (new-symbs-make-l (mapcar (lambda (slot) (turn-into-accessor-l (if (listp slot) (car slot) slot))) new-body))
             (struct-creator   (lol:symb (concatenate 'string "MAKE-" (lol:mkstr name))))
             (struct-creator-l (lol:symb (concatenate 'string "MAKE-" (lol:mkstr name) "-L")))
             (value (gensym))
             ;; (func (gensym))
             )

        `(prog2 ;; (declaim (notinline ,@new-symbs-make))
                                        ; we need to notinline things or else the old calls inside a (λ ())
             ,(if doc-string
                  `(defstruct ,name-and-options ,doc-string ,@new-body)
                  `(defstruct ,name-and-options ,@new-body)) ; of any kind will use the old version, and ruin our code!
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
             ,@(mapcar (lambda (maker-symb-l maker-symb)
                         `(defun ,maker-symb-l (,name)
                            (let ((,value (,maker-symb ,name)))
                              (if (lazy-p ,value)
                                  (setf (,maker-symb ,name) (force ,value))
                                  ,value))))
                       new-symbs-make-l new-symbs-make)

           ;; (declaim (inline ,@new-symbs-make))
                                        ; now we can inline the newly updated function
           (defmacro ,struct-creator-l (&key ,@new-symbs-with-default)
             "creates a lazy version of the struct, delaying all the arguments"
             (list ',struct-creator ,@(mapcar (lambda (x) ; (list '...) because we are doing ` expansion by hand
                                                (if (keywordp x) ; as we need to compile into a defmacro form
                                                    x ; and double ` would add extra , which is uneeded
                                                    `(list 'delay ,x))) ; make the elements delayed, as we want them to be lazy
                                              (lol:flatten (mapcar (lambda (symb)
                                                                     (list (turn-into-key-word symb) symb))
                                                                   new-symbs))))))))))


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