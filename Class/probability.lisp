(defmacro curry (fn . args)
  "Creates a partially applied function that takes 1 argument"
  (let ((arg (gensym)))
    `(lambda (,arg) (,fn ,@args ,arg))))

(defun range (max &optional (min 0) (step 1))
  (loop for x from min to max by step
     collect x))

(defun fact (num &optional (bot 1))
  (reduce #'* (range num bot)))

(defun choose (num group)
  (/ (fact num (1+ (- num group)))
     (fact group)))

(defun combinations! (num group)
  (/ (fact num) (fact (- num group)) (fact group)))


(= (/ (fact 5) (* (fact (- 5 3)) (fact 3))) (choose 5 3))


(defun dearrange% (k)
  (cond ((= k 1) 0)
        ((= k 0) 1)
        (t (apply #'- (cons (fact k) (mapcar (lambda (x)
                                               (* (choose k x) (dearrange x)))
                                             (range (- k 1))))))))

(defun dearrange (k)
  (case k (1 0)
          (0 1)
          (t (apply #'- (cons (fact k) (mapcar (lambda (x)
                                                 (* (choose k x) (dearrange x)))
                                               (range (- k 1))))))))

(defun prob-of-no-two-occur (sample-space total-events)
  (/ (fact sample-space)
     (fact (- sample-space total-events))
     (expt sample-space  total-events)))


(defun sum-of-chance-of-not-occuring (x)
  (reduce #'+ (mapcar (curry prob-of-no-two-occur x) (range x 1))))

(defun proper-measure (sample-space k)
  (/ (prob-of-no-two-occur sample-space k)
     (sum-of-chance-of-not-occuring sample-space)))

(reduce #'+ (mapcar (lambda (x) (proper-measure 24 x)) (range 24 1)))


(defun test (n k j)
  (/ (* (- n 1) j) n (- n k 1)))

(defun test2 (n k j)
  (/ (* (- n j) j) n (- n k 1)))

(defun test? (n k j)
  (< (/ j n) (test n k j)))


(defun l-less (i)
  (exp (- (/ (expt 2 i) (expt 3 (1+ i))))))

(defun l-equal (i)
  (- (exp (- (/ (expt 2 (1+ i)) (expt 3 (+ 2 i)))))
     (exp (- (/ (expt 2 i)     (expt 3 (1+ i)))))))

(defun fair-l (i)
  (exp (- (/ 1 (expt 2 (1+ i))))))

(defun b^2>=4ac ()
  (let ((a (/ (random 100000000000) 100000000000.0))
        (b (/ (random 100000000000) 100000000000.0))
        (c (/ (random 100000000000) 100000000000.0)))
    (when (>= (expt b 2) (* 4 a c))
      (list a b c))))

(defparameter *passed-values-a* nil)
(defparameter *passed-values-b* nil)
(defparameter *passed-values-c* nil)
;; (let ((count 0)) (dotimes (i 10000) (setf count (+ count (b^2>=4ac)))))
(defun list-to-2d-array (list)
  (make-array (list (length list)
                    (length (first list)))
              :initial-contents list))

(let ((count 0)) (dotimes (i 10000000) (let ((ans (b^2>=4ac)))
                                         (when ans
                                           (incf count)
                                           (push (car ans) *passed-values-a*)
                                           (push (cdr ans) *passed-values-b*)
                                           (push (list (cadr ans) (cadr ans)) *passed-values-c*)))) count)

(defparameter x (coerce *passed-values-a* 'vector))
(defparameter y (coerce *passed-values-b* 'vector))

;; (defun list-to-2d-array (list)
;;   (make-array (list (length list)
;;                     (length (first list)))
;;               :initial-contents list))

;; (defparameter z (coerce *passed-values-c* 'vector))


;; (defparameter z (list-to-2d-array *passed-values-c*))

;; (let ((count 0)) (dotimes (i 100000000) (setf count (+ count (b^2>=4ac)))) (/ count 100000000.0))
(/ (random 100000000000) 100000000000.0)
(/ (random 100000000000) 100000000000.0)

;; (let ((c (cl-plplot:new-3d-mesh #2(1 2) #2( 2 3) #2a((1 2) (1 2))))
;;       (w (cl-plplot:basic-3d-window)))
;;   (cl-plplot:add-plot-to-window w c)
;;   (cl-plplot:render w "xwin"))
