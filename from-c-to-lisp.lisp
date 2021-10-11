;; /* binseaerch: find x in v[0] <= v[1] <= ... <= v[n-1] */
;; int binsearch (int x, int v[], int n)
;; {
;;     int low  = 0;
;;     int high = n - 1;
;;     int mid  = high / 2;

;;     while (low <= high) {
;;         if (x < v[mid])
;;             high = mid - 1;
;;         else
;;             low = mid + 1;
;;         mid = (low + high) / 2;
;;     }
;;     return (x == v[mid]) ? mid : -1;
;; }

;; Translated from my K&R c
(defun binsearch% (num arr &optional (length (length arr)))
  "Finds num in arr, and returns the location where it is found, the array has to be sorted"
  (let* ((high (1- length))
         (low 0)
         (mid (floor (/ high 2))))
    (loop :while (<= low high)
          :do (progn (if (< num (elt arr mid))
                         (setf high (1- mid))
                         (setf low  (1+ mid)))
                     (setf mid (floor (/ (+ low high) 2)))))

    (if (= (elt arr mid) num) mid -1)))

;; Done the lisp way
(defun binsearch (num arr &optional (length (length arr)))
  "Finds num in arr, and returns the location where it is found, the array has to be sorted"
  (labels ((rec (high low &optional (mid (floor (/ (+ low high) 2))))
             (cond ((> low high)          mid)
                   ((< num (elt arr mid)) (rec (1- mid) low))
                   (t                     (rec high (1+ mid))))))
    (let ((mid (rec (1- length) 0)))
      (if (= (elt arr mid) num) mid -1))))
