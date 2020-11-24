(defclass queue ()
  ((list :initform nil)
   (tail :initform nil)))

(defstruct monster (health 10))

;; Used for the look of the struct
(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type t)
    (with-slots (list tail) queue
      (cond ((cddddr list)
             ;; 5 elements so print ellipsis
             (format stream "(~{~S ~}... ~S)"
                     (subseq list 0 3) (car tail)))
            (t (format stream "~:S" list))))))

(defmethod dequeue ((queue queue))
  (with-slots (list) queue
    (pop list)))

(defmethod enqueue (new-item (queue queue))
  (with-slots (list tail) queue
    (let ((new-tail (list new-item)))
      (cond ((null list)
             (setf list new-tail))
            (t (setf (cdr tail) new-tail)))
      (setf tail new-tail)))
  queue)


(make-instance 'queue)
