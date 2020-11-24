(ql:quickload "cl-plplot")

(defparameter *a* 3)

(defpackage #:plot
  (:nicknames #:pl)
  (:use #:cl-plplot
        #:common-lisp))

(defun range (max &optional (min 0) (step 1))
  (loop for x from min to max by step
     collect x))

(in-package #:plot)
(defun generate-x (number)
  (let* ((curr (* *a* (/ (random 100000000000) 100000000000.0 )))
         (list curr))
    (cons list (loop repeat (1- number) collect (setf curr (* curr (/ (random 100000000000) 100000000000) *a*))))))

(defun generate-test (number)
  (let* ((curr (* *a* .5))
         (list curr))
    (cons list (loop repeat (1- number) collect (setf curr (* curr .5 *a*))))))

(defun x-y-plot (num)
  (let* ((graph (generate-x num))
         (y1 (apply #'vector (generate-x num) ;; (mapcar (lambda (x) (if (<= 10 x) 0 x)) (generate-x num))
                    ))
         (y2 (map 'vector #'log y1))
	 (x (apply #'vector (range (1- num))))
	 (p1 (new-x-y-plot x y1))
         (p2 (new-x-y-plot x y2))
	 (w1 (basic-window))
         (w2 (basic-window)))
    (add-plot-to-window w1 p1)
    (add-plot-to-window w2 p2)
    (cl-plplot:render w1 "xwin")
    (cl-plplot:render w2 "xwin")
    (print y1)
    y2))

(defun round-to (number precision &optional (what #'round))
    (let ((div (expt 10 precision)))
         (/ (funcall what (* number div)) div)))
