(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:trivia
                  :fset))
  (rename-package 'fset 'fset '(:fs))
  (use-package 'trivia))

;; Leftist heaps **************************************************************************************************

(defconstant empty-heap :empty)

(defstruct heap
  (rank 1)
  value
  (left empty-heap :type (or heap symbol))
  (right empty-heap :type (or heap symbol)))


(defun heap-empty-p (heap)
  (equalp heap empty-heap))

(defun heap-merge (x-heap y-heap &key (compare #'<))
  (cond ((heap-empty-p x-heap) y-heap)
        ((heap-empty-p y-heap) x-heap)
        ((funcall compare (heap-value x-heap) (heap-value y-heap))
         (make-t (heap-value x-heap)
                 (heap-left  x-heap)
                 (heap-merge (heap-right x-heap) y-heap)))
        (t
         (make-t (heap-value y-heap)
                 (heap-left y-heap)
                 (heap-merge x-heap (heap-right y-heap))))))

(defun rank (heap)
  (if (heap-empty-p heap)
      0
      (heap-rank heap)))

(defun make-t (val xs ys)
  (if (>= (rank xs) (rank ys))
      (make-heap :rank (1+ (rank ys)) :value val :left xs :right ys)
      (make-heap :rank (1+ (rank xs)) :value val :left ys :right xs)))

(defun heap-insert (x heap)
  (heap-merge (make-heap :value x) heap))

(defun find-min (heap)
  (heap-value heap))

(defun remove-min (heap)
  (heap-merge (heap-left heap) (heap-right heap)))


;; Example Usage

(defparameter *x* (make-heap :rank 2 :value 1 :left  (make-heap :value 2)
                                              :right (make-heap :value 3)))


(defparameter *y* (heap-merge (heap-merge (make-heap :value 4)
                                          (make-heap :value 3))
                              (heap-merge (make-heap :value 1)
                                          (make-heap :value 2))))
