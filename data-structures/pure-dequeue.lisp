(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:trivia))
  (use-package 'trivia))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct dequeue
    (size-f 0 :type Integer) (size-e 0 :type Integer)
    (front '() :type list)   (end '() :type list)))

(defun consl (val dequeue)
  "cons on an element to the front of the dequeue"
  (make-dequeue :size-e (dequeue-size-e dequeue)
                :size-f (1+ (dequeue-size-f dequeue))
                :end    (dequeue-end dequeue)
                :front  (cons val (dequeue-front dequeue))))

(defun consr (val dequeue)
  "cons on an element to the back of the dequeue"
  (make-dequeue :size-e (1+ (dequeue-size-e dequeue))
                :size-f (dequeue-size-f dequeue)
                :end    (cons val (dequeue-end dequeue))
                :front  (dequeue-front dequeue)))


(defun-match cdrl (dequeue)
  ((Dequeue :size-f 0 :size-e 0)
   dequeue)                               ; just return the empty dequeue, instead of returning an error
  ((Dequeue :size-f 0 :size-e e :end end) ; if the front is empty, we need to reverse the end list to get the front list
   (let* ((front-size (1- (ceiling (/ e 2)))) ; we remove the first element of the front
          (end-size   (floor (/ e 2))) ; the size of the elements that stay inside of end
          (splited    (split-at end-size end))
          (end-list   (car splited))
          (front-list (cadr splited)))
     (make-dequeue :size-e end-size
                   :size-f front-size
                   :front  (cdr (reverse front-list))
                   :end    end-list)))
  ((Dequeue :size-f f :size-e e :front front :end end)
   (make-dequeue :size-e e
                 :size-f (1- f)
                 :front  (cdr front)
                 :end    end)))

(flet ((flip (dequeue)
         (make-dequeue :size-e (dequeue-size-f dequeue)
                       :size-f (dequeue-size-e dequeue)
                       :front  (dequeue-end    dequeue)
                       :end    (dequeue-front  dequeue))))
  (defun cdrr (dequeue)
    (let ((removed (cdrl (flip dequeue)))) ; remove the right most thing by removing the left thing in the flipped
      (flip removed))))


(defun split-at (x lis &optional acc)
  (if (or (zerop x) (null lis))
      (list (reverse acc) lis)
      (split-at (1- x) (cdr lis) (cons (car lis) acc))))