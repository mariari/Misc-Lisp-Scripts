;; Chapter 5----------------------------------------------------------
(defun fif (if then &optional else)
  "Checks the given parameter for a function and acts accordingly"
  #'(lambda (x)
      (if (funcall if x)
          (funcall then x)
          (if else (funcall else x)))))
;; Useage
;; (mapcar (fif #'oddp (lambda (x) (< 10 x)) #'evenp)
;;         '(1 2 3 4 5 6 ))

;; -> nil t nil t nil t
;; (mapcar (fif #'oddp #'- #'evenp)
;;         '(1 2 3 4 5 6 ))

;;  -> -1 T -3 T -5 T
;; Chapter 4----------------------------------------------------------
(defun group (source n)
  "creates lists with n elements from a list(source)"
  (when (zerop n)
      (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))


(defun flatten (x)
  "Completely flattens a given list"
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun flatten-o (x &optional (acc nil))
  "Completely flattens a given list"
  (cond ((null x) acc)
        ((atom x) (cons x acc))
        (t (flatten-o (car x) (flatten-o (cdr x) acc)))))




(defun collatz (n)
  (cond ((<= n 1) n)
        ((eq (mod n 2) 0)
         (cons n (collatz (/ n 2))))
        ((eq (mod n 2) 1)
         (cons n (collatz (+ (* 3 n) 1))))
        (t)))

(collatz (expt 27 10))



(defun compose (&rest fns)
  "Returns a function like F(G(x)).... the functions,
   when applied happen in reverse order of how they were inputted"
  (if fns
      (let ((fn1 (car (last fns)))
            (fns (butlast fns)))
        (lambda (&rest args)
          (reduce #'funcall fns
                  :from-end t
                  :initial-value (apply fn1 args))))
      #'identity))


;; Testing compusing F(g(x))
(compose #'list #'1+)
(funcall (compose #'list #'1+ ) 2 )

(funcall (compose #'list #'+) 2 3 4)

;; We might as well just use apply and list outside of this 
(funcall (compose #'list #'apply) #'+ '(2 3 4))

;; = (9) = (list (apply #'+ '(2 3 4)))



(funcall (compose #'1+ #'find-if) #'oddp '(2 3 4 5))
;; above is eq --> (1+ (find-if #'oddp '(2 3 4 5)))
