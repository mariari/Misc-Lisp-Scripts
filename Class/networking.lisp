
(defun estimated-rtt (xs α prev)
  (reduce (lambda (estimatedRTT-prev x)
            (+ (* (- 1 α) estimatedRTT-prev)
               (* α x)))
          xs :initial-value prev))

(defun dev-rtt (xs β prev &optional (α .125) (prev-est))
  (car (reduce (lambda (acc x)
                 (list (+ (* (- 1 β) (car acc))
                          (* β (abs (- x (cadr acc)))))
                       (estimated-rtt (list x) α (cadr acc))))
               xs :initial-value (list prev prev-est))))

(defun timeout-interval (xs prev-rtt prev-est &optional (α .125) (β .25))
  (+ (estimated-rtt xs α prev-est)
     (* 4 (dev-rtt xs β prev-rtt α prev-est))))