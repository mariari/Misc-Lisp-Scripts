(defun pixel-count (image &optional (pixel 24) &aux (max (1- (expt 2 pixel))))
  (loop :with white = 0 and black = 0
     :for i fixnum :across image
     :if (zerop i) :do (incf white)
     :if (= max i) :do (incf black)
     finally (return (cons white black))))


(if 2
    (+ 1 2 3 4)
    (+ 1 2 2 3))
