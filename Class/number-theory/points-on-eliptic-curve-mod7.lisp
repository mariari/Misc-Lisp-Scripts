(defun gen-points (f)
  (let ((squared-points '((0 0) (1 1) (2 4) (3 2))))
    (remove-if
     #'null
     (mapcar (lambda (x)
               (let ((y-value
                      (caar (member (mod (funcall f x) 7) squared-points :key #'cadr))))
                 (list x y-value (and y-value (- 7 y-value)))))
             (f:range 0 6))
     :key #'cadr)))
