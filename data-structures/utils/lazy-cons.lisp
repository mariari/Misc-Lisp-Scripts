;;; functions built ontop of laziness************************************************************************************

(defun sterms (n)
  (scons (/ 1 (expt n 2))
         (sterms (1+ n))))

(defun sieve (lis)
  (scons (scar lis)
         (sieve (sfilter (lambda (x) (/= 0 (mod x (scar lis))))
                         (scdr lis)))))

(defun primes ()
  (labels ((naturals-from (n)
             (scons n (naturals-from (1+ n)))))
    (sieve (naturals-from 2))))

(defparameter *test2* (scons 1 (scons 2 nil)))

(scdr (scdr *test2*))

(defparameter *sterms* (sterms 1))

;; the first 100 values of sterm are evaluated
;; this function is just iterate in disguise and an end point
(reduce (lambda (acc _)
          (declare (ignore _))
          (scar acc)
          (scdr acc))
        (make-list 101) ; should just put range into my auto load file :(
        :initial-value *sterms*)

;; time to add a match pattern to this

(defun insert (x lis)
  (cond ((null lis)        (scons x '()))
        ((<= x (scar lis)) (scons x lis))
        (t                 (scons (scar lis) (insert x (scdr lis))))))
