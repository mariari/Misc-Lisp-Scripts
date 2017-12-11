;; This is the list overflowing problem
;; suppose we have a list, and we graphed it as a bar graph
;; now suppose that we wanted to see how many units of water
;; this structure can hold. Guy Steele likes to talk about this problem a fair
;; Here is my solution
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload 'trivia)
  (use-package 'trivia))

(defun water-sustained (xs)
  (let* ((max-l (max-left xs))
         (max-r (max-right max-l :key #'car)))
    (reduce (lambda (acc x)
              (match x
                ((list (list val max-left) max-right)
                 (let ((min (min max-left max-right)))
                   (+ acc
                      (if (>= min val) (- min val) 0))))))
            max-r :initial-value 0)))


(defun max-gen (xs &key (key #'identity))
  (reduce (lambda (ys x)
            (match ys
              ((list* (list val max) _)
               (cons (list x (if (> max (funcall key val))
                                 max (funcall key val)))
                     ys))))
          (cdr xs)
          :initial-value (list (list (car xs)
                                     (funcall key (car xs))))))

(defun max-left  (xs &key (key #'identity)) (reverse (max-gen xs :key key)))
(defun max-right (xs &key (key #'identity)) (max-gen (reverse xs) :key key))