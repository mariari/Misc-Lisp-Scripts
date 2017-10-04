
(defun flip-cake (&rest degree-to-flip)
  "flips a cake by an integer or a list of integers,
   the cake is properly flipped when the frosting is back on the top"
  (let ((flip-value (reduce #'+ degree-to-flip)))
    (labels ((flipped-p     (num)               (= 0 (mod num 720)))
             (flip-until-up (flip-num cake-num) (if (flipped-p cake-num)
                                                    flip-num
                                                    (flip-until-up (1+ flip-num)
                                                                   (+ cake-num flip-value)))))
      (flip-until-up 1 flip-value))))

(defun flip-cake-int (&rest degree-to-flip)
  "if all the numbers are integers... just call this"
  (let ((flip-value (reduce #'+ degree-to-flip)))
    (/ (lcm flip-value 720)
       flip-value)))

;;  converts any number to integers, so very poor!!
(defun flip-cake% (&rest degree-to-flip-list)
  "first attempt at flipping the cake"
  (let ((cake-frost (make-array 360 :element-type 'bit
                                :initial-element 1)) ; 1 is frosting on top
        (number-of-flips 0)                          ; the total number of times we need to flip
        (last-spot       0))                         ; set the last part of the cake we flipped the frosting
    (labels ((frosting-on-top-p ()  (= 360 (reduce #'+ cake-frost)))
             (toggle            (n) (if (zerop n) 1 0))
             (flip (num)
               "flips the cake"
               (loop :for x :from last-spot :to (1- (+ num last-spot))
                  :do      (setf #1=(elt cake-frost (mod x 360)) (toggle #1#))
                  :finally (setf last-spot (mod x 360)))))
      (loop (mapc (lambda (x)
                    (incf number-of-flips)
                    (flip x)
                    (when (frosting-on-top-p)
                      (return number-of-flips)))
                  degree-to-flip-list)))))
