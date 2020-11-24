(defun add-char (x num)
  (let ((x-num (- (char-code x) 90)))
    (code-char (+ 65 (mod (+ -1 num x-num) 26)))))

(defun add-char-no-space (x num)
  (let ((x-num (- (char-code x) 90)))
    (if (or (eq #\newline x) (eq #\space x))
        #\space
        (code-char (+ 65 (mod (+ -1 num x-num) 26))))))
