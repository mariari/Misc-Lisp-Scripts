(defparameter *big* 100)

(defparameter *small* 1)

(defun guess-my-number ()
  (ash (+ *small* *big*) -1))

 (defun smaller ()
   (setf *big* (- (guess-my-number) 1))
   (guess-my-number))


(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

;; (defun bigger ()
;;   (setf *small* (+ 1 (guess-my-number)))
;;   (guess-my-number))

(defun start-over ()
  (setf *big* 100)
  (setf *small* 1)
  (guess-my-number))
