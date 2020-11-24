;; Making a let faster by telling the compiler things about the variables

(defun register-allocated-fixnum ()
  (declare (optimize (speed 3) (safety 0)))
  ;; Does not use pointers; stores it directly in the cpu registers
  (let ((acc 0))
    (loop for i from 1 to 100 do
         (incf (the fixnum acc)
               (the fixnum i)))
    acc))

(defun not-register-allocated-fixnum ()
  ;; Uses pointers 
  (let ((acc 0))
    (loop for i from 1 to 100 do
         (incf acc i))
    acc))


;; a closure with 2 functions
(defun double-closure ()
  (let ((counter 0))
    (values
     (lambda () (incf counter))
     (lambda () (decf counter)))))

(multiple-value-bind (x y)
    (double-closure)
    (list x y))

;; an anon class 
(defun counter ()
  (let ((counter 0))
    (lambda () (incf counter))))

(defun block-scanner-s (trigger-string)
  (let* ((trig (coerce trigger-string 'list))
         (curr trig))
    (lambda (data-string)
      (let ((data (coerce data-string 'list)))
        (dolist (c data)
          (when curr
            (setq curr (if (char= (car curr) c)
                           (cdr curr)   ; next char
                           trig))))     ; Start over
        (not curr)))))

(defun block-scanner (trigger-string)
  (let* ((trig (coerce trigger-string 'list))
         (curr trig))
    (lambda (data-string)
      (when curr (loop for c across data-string
                 when curr do
                   (setq curr (if (char= (car curr) c)
                                  (cdr curr) ; next char
                                  trig))))   ; start over
      (not curr))))

(defun block-scanner-c (trigger-string)
  (let* ((trig (coerce trigger-string 'list))
         (curr trig)
         (counter 0))
    (lambda (data-string)
      (loop for c across data-string
         do
           (setq curr
                 (cond ((null curr) (incf counter) trig)
                       ((char= (car curr) c) (cdr curr)) ; next char
                       (t trig))))      ; reset to trig
      (if (null curr)
          (progn  (setq curr trig)
                  (incf counter))
          counter))))

(let ((direction 'up))
  (defun toggle-counter-direction ()
    (setq direction
          (if (eq direction 'up)
              'down
              'up)))

  (defun counter-class ()
    (let ((counter 0))
      (lambda ()
        (if (eq direction 'up)
            (incf counter)
            (decf counter))))))
