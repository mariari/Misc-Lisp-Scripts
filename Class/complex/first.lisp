
(defstruct polar r theta)

(defun to-polar (x)
  (make-polar :r (abs x) :theta (phase x)))
