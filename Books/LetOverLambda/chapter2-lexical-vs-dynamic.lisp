(defparameter *dyn-a* 10)

(let ((lex-test 3))
  (setf lex-test 5)
  (defun lexical-test (x)
    (list x lex-test)))

(let ((lex-test 3))
  (let ((lex-test 5))
    (defun lexical-test2 (x)
      (setf *dyn-a* 30)
      (list x lex-test *dyn-a*))))


(defparameter *dyn-test* 1)

(defun dyn-test-1 (x)
    (list x *dyn-test*))

(let ((*dyn-test* 3))
  (setf *dyn-test* 5)
  (defun *dyn-test* (x)
    (list x *dyn-test*)))




(let ((lex-test 6))
  (lexical-test 2))

;; (2 5)

(let ((*dyn-test* 9))
  (*dyn-test* 2))


(let ((*dyn-test* 9)) (dyn-test-1 2))

;; (2 9)


(let ((lex-test 6)
      (*dyn-a* 20))
  (lexical-test2 2))

;; (2 5)

(defun test (a)
  (setf a 2))
