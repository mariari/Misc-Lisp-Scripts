(defpackage #:assembly
  (:documentation "assembly utilities")
  (:use #:common-lisp)
  (:export #:emit-long-nop))

(in-package :assembly)

;; we are using the ∑k = n(n + 1) / 2. to determine boundaries
(defun emit-long-nop (amount)
  ;; Pack all instructions into one byte vector to save space.
  (let* ((bytes #.(coerce
                   #(#x90
                    #x66 #x90
                    #x0f #x1f #x00
                    #x0f #x1f #x40 #x00
                    #x0f #x1f #x44 #x00 #x00
                    #x66 #x0f #x1f #x44 #x00 #x00
                    #x0f #x1f #x80 #x00 #x00 #x00 #x00
                    #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00
                    #x66 #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00)
                   '(vector (unsigned-byte 8))))
         (max-length (isqrt (* 2 (length bytes))))
         (collection (make-array amount
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0
                                 :fill-pointer 0)))
    (loop
      (let* ((count (min amount max-length))
             ;; Here we start the summation at 1 less. n(n -1) / 2
             (start (ash (* count (1- count)) -1)))
        (dotimes (i count)
          (vector-push (aref bytes (+ start i)) collection)))
      (if (> amount max-length)
          (decf amount max-length)
          (return collection)))))
