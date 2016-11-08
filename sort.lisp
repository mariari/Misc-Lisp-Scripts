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

(defun merge-sort (sequence)
  
  (if (= (length sequence) 1)
      sequence
      (let ((half (truncate (/ (length sequence) 2))))
        (merge (type-of sequence)
               (merge-sort (subseq sequence 0 half))
               (merge-sort (subseq sequence half))
               #'<))))


(defun my-merge-sort (sequence)
  (labels (merge-them))
  (if (= (length sequence) 1)
      sequence
      (let ((half (truncate (/ (length sequence) 2))))
        ;; MERGE is a standard common-lisp function, which does just
        ;; what we want.
        (merge-them (subseq sequence 0 half)))))


