(defstruct ref contents)

(defun ref (x)
  (make-ref :contents x))

(defun ! (ref)
  (ref-contents ref))

(defun := (ref x)
  "sets the "
  (setf (ref-contents ref) x))

(defun (setf !) (x ref)
  (:= ref x))
