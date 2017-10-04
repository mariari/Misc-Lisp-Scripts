
(defun range (first &optional (second nil) (step 1))
  (macrolet ((for (second word first)
               `(loop :for x :from ,second ,word ,first by step
                   collect x)))
    (cond ((and second (> second first)) (for first to     second))
          (second                        (for first downto second))
          (t                             (for 0     to     first)))))

(defun suffixes (xs)
  (if (null xs)
      nil
      (cons xs (suffixes (cdr xs)))))

(defun suffixes% (xs &optional (cps #'identity))
  (if (null xs)
      (funcall cps xs)
      (suffixes% (cdr xs) (lambda (x) (funcall cps (cons xs x))))))

(defun suffixes%% (xs &optional (acc '()))
  (if (null xs)
      (reverse acc)
      (suffixes%% (cdr xs) (cons xs acc))))

(room)
;; (time (suffixes%% (range 100)))
;; (print (sb-kernel::dynamic-usage))
