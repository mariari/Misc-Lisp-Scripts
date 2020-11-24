;;; Introduction to Lisp
(member 'das '(da ba sa so))

;; Cool function that applies append to mapcar

(defun mappend (fn list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn list)))

(defun self-and-double (x) (list x (+ x x)))

(self-and-double 3)
;; (3 6)

(mapcar #'self-and-double '(1 10 300))
;; ((1 2) (10 20) (300 600))
(mappend #'self-and-double '(1 10 300))
;; (1 2 10 20 300 600)

(defun numbers-and-negations (input)
  "Given a list, returns only the numbers and their negations"
  (mappend #'number-and-negation input))

(defun number-and-negation (x)
  "If x is a number, return a list of x and -x."
  (if (numberp x)
      (list x (- x))
      nil))

;; My first foolish attempt 
(defun count-anywhere% (atom list)
  (let ((acc 0)) 
    (labels ((tails (atom list)
               (if (null list)
                   acc
                   (progn (if (listp (car list))
                              (tails atom (car list))
                              (if (equal atom (car list))
                                  (incf acc)))
                          (tails atom (cdr list))))))
      (tails atom list))))

;; Let over Lambda has failed me 
(let ((acc 0))
  (defun count-anywhere%% (atom list)
    (if (null list)
        acc
        (progn (if (listp (car list))
                   (count-anywhere%% atom (car list))
                   (if (equal atom (car list))
                       (incf acc)))
               (count-anywhere%% atom (cdr list))))))

;; The books way of doing it, so much more elegant.. mine is a mess

(defun count-anywhere (item tree)
  "Count the times item appears anywhere within a tree "
  (cond ((eql item tree) 1)
        ((atom tree) 0)
        (t (+ (count-anywhere item (car tree))
              (count-anywhere item (cdr tree))))))

(count-anywhere 'a '(a b c d a a  (a (b (c (d a))))))
;; 5 

(defun dot-product (a b)
  "Compute the mathematical dot product of two vectors."
  (apply #'+ (mapcar #'* a b)))
