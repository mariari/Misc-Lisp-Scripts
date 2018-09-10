;; This is the list overflowing problem
;; suppose we have a list, and we graphed it as a bar graph
;; now suppose that we wanted to see how many units of water
;; this structure can hold. Guy Steele likes to talk about this problem a fair
;; Here is my solution
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:fare-quasiquote-readtable
                  :trivia.quasiquote
                  :trivia))
  (use-package 'trivia))

(named-readtables:in-readtable :fare-quasiquote)

(defun water-sustained (xs)
  (let* ((max-l (max-left xs))
         (max-r (max-right max-l :key #'car)))
    (reduce (lambda (acc x)
              (match x
                (`((,val ,max-left) ,max-right)
                  (+ acc (max (- (min max-left max-right) val) 0)))))
            max-r :initial-value 0)))

(defun max-gen (xs &key (key #'identity))
  (reduce (lambda (ys x)
            (match ys
              ((cons `(,val ,max) _)
               (cons `(,x ,(max max (funcall key val))) ys))))
          (cdr xs)
          :initial-value `((,(car xs) ,(funcall key (car xs))))))

(defun max-left  (xs &key (key #'identity)) (reverse (max-gen xs :key key)))
(defun max-right (xs &key (key #'identity)) (max-gen (reverse xs) :key key))
