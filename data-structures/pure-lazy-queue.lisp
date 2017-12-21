(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload 'trivia)
  (use-package 'trivia)

  (defstruct-l queue
    (f      nil :type list)
    (f-size 0   :type Integer)
    (e      nil :type list)
    (e-size 0   :type Integer)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *ARITY-CHECK-BY-TEST-CALL* nil))


(declaim (ftype (function (queue) queue) check))
(defun-match check (queue)
  ((queue f f-size e e-size)
   (if (>= f-size e-size)
       queue
       (make-queue :f-size (+ f-size e-size)
                   :f      (sappend f (sreverse e))
                   :e-size 0))))

(defun empty-p (queue)
  (zerop (queue-f-size queue)))

(declaim (ftype (function (t queue) queue) snoc))
(defun snoc (ele queue)
  (match queue
    ((queue f f-size e e-size)
     (check (make-queue :f f            :f-size f-size
                        :e (cons ele e) :e-size (1+ e-size))))))

(declaim (ftype (function (queue) t) q-car))
(defun q-car (queue)
  (scar (queue-f queue)))

(declaim (ftype (function (queue) queue) q-cdr))
(defun-match q-cdr (queue)
  ((queue f f-size e e-size)
   (check (make-queue :f (scdr f) :f-size (1- f-size)
                      :e e        :e-size e-size))))

(defparameter *example-queue* (reduce #'snoc (f:range 1000) :from-end t :initial-value (make-queue)))
