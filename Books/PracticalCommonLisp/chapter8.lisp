(defun primep (number)
  (when (> number 1)
    (loop for fac from 2 to (isqrt number) never (zerop (mod number fac)))))

(defun next-prime (number)
  (loop for n from number when (primep n) return n))


(do ((p (next-prime 0) (next-prime (1+ p))))
    ((> p 19))
  (format t "~d" p))

;; This code leaks if you pass random number to the end variable because it is evaluated more than once 
(defmacro do-primers% ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ,end))
     ,@body))

;; Creates a leak when ending-value is passed to it. Also there is a leak in the order of evaluation, matters if there are side effects
(defmacro do-primers%% ((var start end) &body body)
  `(do ((ending-value ,end)
        (,var (next-prime ,start) (next-prime (1+ ,var))))
       ((> ,var ending-value))
     ,@body))

(defmacro do-primers%%% ((var start end) &body body)
  `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
        (ending-value ,end))
       ((> ,var ending-value))
     ,@body))
;; Works perfectly fine... no issues 
(defmacro do-primers%%%% ((var start end) &body body)
  (let ((ending-value-name (gensym)))
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))


(do-primers (p 0 19)
  (format t "~d" p))


(do-primers%%%%% (p 0 (random 100)) (format t "~d" p))


(loop for n in '(a b c) collect `(,n (gensym)))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(mapcar (lambda (x) `(,x (gensym))) names)
     ,@body))

(defmacro do-primers%%%%% ((var start end) &body body)
  (with-gensyms (ending-value-name)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var)))
          (,ending-value-name ,end))
         ((> ,var ,ending-value-name))
       ,@body)))

;; ((A (GENSYM))
;;  (B (GENSYM))
;;  (C (GENSYM)))

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
                ,@body)))))

(defmacro do-primers ((var start end) &body body)
  (once-only (start end)
    `(do ((,var (next-prime ,start) (next-prime (1+ ,var))))
         ((> ,var ,end))
       ,@body)))



(do-primers (p 0 (random 100)) (format t "~d" p))
