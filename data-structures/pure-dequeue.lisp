(setf *ARITY-CHECK-BY-TEST-CALL* nil)

;; we can view all r operations as a flipped version of the l operations
;; so instead of implementing a left and right, we can just implement a single direction and use that

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:trivia))
  (use-package 'trivia))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct dequeue
    (size-f 0  :type Integer)
    (size-e 0  :type Integer)
    (front '() :type list)
    (end   '() :type list)))


(defun consl (val dequeue)
  "cons on an element to the front of the dequeue"
  (let ((size-f (dequeue-size-f dequeue)))
    (if (and (zerop (dequeue-size-e dequeue))
           (>= size-f 2))
        (split-into-dequeue (cons val (dequeue-front dequeue))
                            (1+ size-f))
        (make-dequeue :size-e (dequeue-size-e dequeue)
                      :size-f (1+ (dequeue-size-f dequeue))
                      :end    (dequeue-end dequeue)
                      :front  (cons val (dequeue-front dequeue))))))

(defun consr (val dequeue)
  "cons on an element to the back of the dequeue"
  (flip (consl val (flip dequeue))))

;; break this middle splitting to be its own function and then just call it
;; or maybe just call cdrl grab the value and put it back onto the part, either one works really


; if we cdrr on an end of length 1 then we violate the precondition that
; we violated the precondition that neither list should be non-nil
; just return the empty dequeue, instead of returning an error
(defun-match cdrr (dequeue)
  ((Dequeue :size-f 0 :size-e 0)                        dequeue)
  ((Dequeue :size-f f :size-e 0 :front front)          (let-match1 (dequeue size-e size-f front end)
                                                                   (split-into-dequeue front f)
                                                         (make-dequeue :size-e (1- size-e)
                                                                       :end    (cdr end)
                                                                       :size-f size-f
                                                                       :front  front)))
  ((Dequeue :size-f f :size-e 1 :front front)          (split-into-dequeue front f))
  ((Dequeue :size-f f :size-e e :front front :end end) (make-dequeue :size-e (1- e)
                                                                     :size-f f
                                                                     :front  front
                                                                     :end    (cdr end))))


(defun cdrl (dequeue)
  "does the cdr on the left side of the dequeue."
  (flip (cdrr (flip dequeue))))


(defun carr (dequeue)
  (cond ((and (null (dequeue-front dequeue)) (null (dequeue-end dequeue))) ; if this were any other lang, signal an error
         nil)
        ((not (null (dequeue-end dequeue)))
         (car (dequeue-end dequeue)))
        (t                                ; note we car instead of last because this case only happens when the
         (car (dequeue-front dequeue))))) ; the front is of length 1, if this is not the csae, some precondition was violated

(defun carl (dequeue)
  (carr (flip dequeue)))


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
(defun split-at (x lis &optional acc)
  (if (or (zerop x) (null lis))
      (list (reverse acc) lis)
      (split-at (1- x) (cdr lis) (cons (car lis) acc))))

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