(ql:quickload 'trivia)
(use-package 'trivia)

(defun heap-sort (a &optional (func #'<) (count (length a)))
  "Heap sort, pass a function (#'< or #'>) to sort the final array default is max at end"
  (macrolet ((ref (i) `(aref a ,i))
             (swap (i j) `(rotatef (ref ,i) (ref ,j)))
             (ref< (i j) `(funcall func (ref ,i) (ref ,j))))

    (labels ((sift (root count)
               (let ((cl (+ (* 2 root) 1))
                     (cr (+ (* 2 root) 2)))
                 (if (< cl count)
                     (let ((c (if (and (< cr count) (ref< cl cr)) cr cl)))
                       (when (ref< root c)
                         (swap root c)
                         (sift c count)))))))

      (loop for start from (1- (floor count 2)) downto 0
         do (sift start count))

      (loop for end from (1- count) downto 1
         do (swap 0 end) (sift 0 end))))
  a)


(defun merge-sort (sequence &optional (f #'<))
  "Does not have Side effects"
  (let ((len (length sequence)))
    (if (= len 1)
        sequence
        (let ((half (truncate (/ len 2))))
          (merge (type-of sequence)
                 (merge-sort (subseq sequence 0 half))
                 (merge-sort (subseq sequence half))
                 f)))))

(defun my-merge-sort (sequence &optional (f #'<))
  "Does not have Side effects"
  (let ((len (length sequence)))
    (if (= len 1)
        (coerce sequence 'list)
        (let ((half (truncate (/ len 2))))
          (my-merge
                 (merge-sort (subseq sequence 0 half))
                 (merge-sort (subseq sequence half))
                 f)))))

(defun my-merge (list1 list2 &optional (pred #'<) (cps #'identity))
  (flet ((recur (fn1 fn2 lesser)
           (my-merge (funcall fn1 list1)
                     (funcall fn2 list2)
                     pred
                     (lambda (x) (funcall cps (cons (car lesser) x))))))
    (cond ((null list1)                           (funcall cps list2))
          ((null list2)                           (funcall cps list1))
          ((funcall pred (car list1) (car list2)) (recur #'cdr #'identity list1))
          (t                                      (recur #'identity #'cdr list2)))))

(defun insertion-sort% (l &optional (pred '<=))
  "a variation on insertion sort where elements get appended to the front instead of checking in place
   same O(n^2)"
  (labels ((insert (x ys)
             (match ys
               ((list)                                 (list x))
               ((guard (list* y _) (funcall pred x y)) (cons x ys))
               ((list* y rst)                          (cons y (insert x rst))))))
    (coerce (reduce #'insert l :initial-value '() :from-end t)
            (type-of l))))

(defun insertion-sort (seq &optional (f #'>))
  "Has Side effects"
  (let ((key 0)
        (i 0))
    (dotimes (j (1- (length seq)))
      (setf key (elt seq (1+ j)))
      (setf i j)
      (loop :while (and (> i -1) (funcall f (elt seq i) key))
            :do (progn
                  (setf (elt seq (1+ i)) (elt seq i))
                  (decf i)))
      (setf (elt seq (1+ i)) key)))
  seq)



;; FROM ROSETTA CODE----------------------------------
(defun span (predicate list)
  (let ((tail (member-if-not predicate list)))
    (values (ldiff list tail) tail)))

(defun less-than (x)
  (lambda (y) (< y x)))

(defun insert (list elt)
  (multiple-value-bind (left right)
      (span (less-than elt) list)
    (append left (list elt) right)))

(defun insertion-sort%% (list)
  (reduce #'insert list :initial-value nil))
;;----------------------------------------------------

;; (defun insertion-sort% (deq &optional (f #'>)))

(defun test (type)
  (flet ((test-ab (seq name)
           (format t "-----------------------------------------------------------------~a-----------------------------------------------------------------" name)
           (print "----------------------------------------------------------------MERGE-SORT----------------------------------------------------------------")
           (map 'list (lambda (x) (time (merge-sort x))) seq)
           (print "--------------------------------------------------------------INSERTION-SORT--------------------------------------------------------------")
           (map 'list (lambda (x) (time (insertion-sort x))) seq))
         (make-seq (up-to)
           (coerce  (loop for j from 1 to 10 collect (loop for i from 1 to up-to collect (random 1342))) type)))
    (let* ((a4    (make-seq 4))
           (a16   (make-seq 16))
           (a32   (make-seq 32))
           (a64   (make-seq 64))
           (a256  (make-seq 256))
           (a1024 (make-seq 1024)))
      (test-ab a4 "a4")
      (test-ab a16 "a16")
      (test-ab a32 "a32")
      (test-ab a64 "a64")
      (test-ab a256 "a256")
      (test-ab a1024 "a1024")
      nil)))


(defun test-arr ()
  (test 'vector))

(defun test-list ()
  (test 'list))


(defun test-worst (type)
  (flet ((test-ab (seq name)
           (format t "-----------------------------------------------------------------~a-----------------------------------------------------------------" name)
           (print "----------------------------------------------------------------MERGE-SORT----------------------------------------------------------------")
           (map 'list (lambda (x) (time (merge-sort x))) seq)
           (print "--------------------------------------------------------------INSERTION-SORT--------------------------------------------------------------")
           (map 'list (lambda (x) (time (insertion-sort x))) seq))
         (make-seq (up-to)
           (coerce  (loop for j from 1 to 3 collect (reverse (loop for i from 1 to up-to collect i))) type)))
    (let* ((a4    (make-seq 4))
           (a16   (make-seq 16))
           (a32   (make-seq 32))
           (a64   (make-seq 64))
           (a256  (make-seq 256))
           (a1024 (make-seq 1024)))
      (test-ab a4 "a4")
      (test-ab a16 "a16")
      (test-ab a32 "a32")
      (test-ab a64 "a64")
      (test-ab a256 "a256")
      (test-ab a1024 "a1024")
      nil)))


;; (test-worst 'list)
;; (defun my-merge-sort (sequence)
;;   (labels (merge-them))
;;   (if (= (length sequence) 1)
;;       sequence
;;       (let ((half (truncate (/ (length sequence) 2))))
;;         ;; MERGE is a standard common-lisp function, which does just
;;         ;; what we want.
;;         (merge-them (subseq sequence 0 half)))))
