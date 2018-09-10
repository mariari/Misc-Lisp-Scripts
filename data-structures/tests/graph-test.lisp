(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package 'graph))


(defparameter *nodes* (make-hash-table))

(defun populate-node ()
  (defnode 'A *nodes* 'B 'C 'D 'E)
  (defnode 'B *nodes* 'A 'D 'F)
  (defnode 'C *nodes*  'A 'F)
  (defnode 'D *nodes*  'A 'B 'E 'G)
  (defnode 'D *nodes*  'A 'B 'E 'G)
  (defnode 'D *nodes*  'A 'B 'E 'G)

  (loop for i from 0 to 1000
     :do (apply #'defnode i *nodes* (loop for i from 0 to (random 100) collect (random 1000))))

  (defnode 'B *nodes* 'A 'D))
