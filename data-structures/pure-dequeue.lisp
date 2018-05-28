(setf *ARITY-CHECK-BY-TEST-CALL* nil)

;; we can view all r operations as a flipped version of the l operations
;; so instead of implementing a left and right, we can just implement a single direction and use that

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :trivia)
  (use-package 'trivia))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct dequeue
    (size-f 0  :type Integer)
    (size-e 0  :type Integer)
    (front '() :type list)
    (end   '() :type list)))


(defun consl (val dequeue)
  "cons on an element to the front of the dequeue"
  (let-match1 (dequeue size-f size-e front end)
              dequeue
    (if (and (zerop size-e) (>= size-f 2))
        (split-into-dequeue (cons val front) (1+ size-f))
        (make-dequeue :size-e size-e
                      :size-f (1+ size-f)
                      :end    end
                      :front  (cons val front)))))

(defun consr (val dequeue)
  "cons on an element to the back of the dequeue"
  (flip (consl val (flip dequeue))))

;; break this middle splitting to be its own function and then just call it
;; or maybe just call cdrl grab the value and put it back onto the part, either one works really


; if we cdrr on an end of length 1 then we violate the precondition that
; we violated the precondition that neither list should be non-nil
; just return the empty dequeue, instead of returning an error
(defun-match cdrr (dequeue)
  ((Dequeue :size-f 0 :size-e 0)     dequeue)
  ((Dequeue size-f :size-e 0 front)  (cdrr (split-into-dequeue front size-f))) ; split and try the other cases
  ((Dequeue size-f :size-e 1 front)  (split-into-dequeue front size-f))
  ((Dequeue size-f size-e front end) (make-dequeue :size-e (1- size-e)
                                                   :size-f size-f
                                                   :front  front
                                                   :end    (cdr end))))


(defun cdrl (dequeue)
  "does the cdr on the left side of the dequeue."
  (flip (cdrr (flip dequeue))))


;; note we car front instead of last because this case only happens when the
;; the front is of length 1, if this is not the csae, some precondition was violated
;; Also note that if we were in any other language we should signal an error for dequeuing an empty queue
(defun carr (dequeue) (or (car (dequeue-end dequeue)) (car (dequeue-front dequeue))))
(defun carl (dequeue) (carr (flip dequeue)))


;;; adding many things
(defmethod add-manyl ((dequeue dequeue) &rest list)
  (add-seql list dequeue))

(defmethod add-seql (seq (dequeue dequeue))
  (reduce #'consl seq :from-end t :initial-value dequeue))

(defmethod add-manyr ((dequeue dequeue) &rest list)
  (add-seqr list dequeue))

(defmethod add-seqr (seq (dequeue dequeue))
  (reduce #'consr seq :from-end t :initial-value dequeue))

;; helper functions*****************************************************************************************************
(defun split-at-rev (x lis &optional acc)
  "Splits a list at a specified location and reverses the first list"
  (if (or (zerop x) (null lis))
      (list acc lis)
      (split-at-rev (1- x) (cdr lis) (cons (car lis) acc))))

(defun split-at (x list &optional acc)
  (let ((split-rev (split-at-rev x list acc)))
    (cons (reverse (car split-rev)) (cdr split-rev))))

(defun flip (dequeue)
  (make-dequeue :size-e (dequeue-size-f dequeue)
                :size-f (dequeue-size-e dequeue)
                :end    (dequeue-front  dequeue)
                :front  (dequeue-end    dequeue)))

(defun split-into-dequeue (list length)
  "splits a list into a queue of two equal parts with the front getting 1 more element than the back"
  (let* ((end-size   (ceiling (/ length 2)))        ; the first half of the elements ceilinged if even
         (front-size (floor (/ length 2)))        ; the second half of the elements floored if odd
         (splited    (split-at front-size list))
         (front-list (car splited))
         (end-list   (cadr splited)))
     (make-dequeue :size-e end-size
                   :size-f front-size
                   :front  front-list
                   :end    (reverse end-list))))
