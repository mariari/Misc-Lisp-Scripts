(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:let-over-lambda :trivia)))

;; EXTRA DEPENDENCEIS :: we don't require lazy, but this is made
;; thus


;; so far this macro only supports the :type keyword
;; I'll add support for the conc later
(defmacro defstruct-l (name-and-options &rest slot-descriptions)
  (flet ((turn-into-accessor (symb) (lol:symb (concatenate 'string
                                                           (lol:mkstr name-and-options)
                                                           "-"
                                                           (lol:mkstr symb))))
         (turn-into-key-word (symb) (intern (concatenate 'string (lol:mkstr symb)) "KEYWORD")))

    (let* ((doc-string (car (remove-if-not #'stringp slot-descriptions)))
           (slot-descriptions (remove-if #'stringp slot-descriptions))
           (new-body
            (mapcar (lambda (slot)
                      (if (not (listp slot))
                          slot
                          (let ((seen-type nil))
                            (mapcar (lambda (x)
                                      (cond (seen-type       (setf x `(or lazy ,x)) )
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
           (struct-creator   (lol:symb (concatenate 'string "MAKE-" (lol:mkstr name-and-options))))
           (struct-creator-l (lol:symb (concatenate 'string "MAKE-" (lol:mkstr name-and-options) "-L")))
           (value (gensym))
           (object (gensym)))

      `(prog1 (defstruct ,name-and-options ,doc-string ,@new-body)
         ,@(mapcar (lambda (maker-symb)
                     `(setf (symbol-function ',maker-symb)
                            (lambda (,object)
                              (let ((,value (,maker-symb ,object)))
                                (if (lazy-p ,value)
                                    (setf (,maker-symb ,object) (force ,value))
                                    ,value)))))
                   new-symbs-make)
         (defmacro ,struct-creator-l (&key ,@new-symbs-with-default)
           (,struct-creator ,@(mapcar (lambda (x) ; make the elements delayed, as we want them to be lazy
                                        (if (keywordp x)
                                            x
                                            `(delay ,x)))
                                      (lol:flatten (mapcar (lambda (symb)
                                                             (list (turn-into-key-word symb) symb))
                                                           new-symbs)))))))))