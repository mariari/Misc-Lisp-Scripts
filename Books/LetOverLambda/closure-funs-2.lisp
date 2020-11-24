(ql:quickload '(:fare-quasiquote-readtable
                :trivia
                :let-over-lambda))

(defpackage #:closure
  ;; (:nicknames)
  (:use #:trivia)
  (:use #:common-lisp))

(in-package :closure)

(named-readtables:in-readtable :fare-quasiquote)


(defun block-scanner-c% (trigger-string)
  (let* ((trig (coerce trigger-string 'list))
         (curr trig)
         (counter 0))
    (lambda (data-string)
      (loop for c across data-string
         do (setq curr
                  (cond ((null curr) (incf counter) trig)
                        ((char= (car curr) c) (cdr curr)) ; next char
                        (t trig))))                       ; reset to trig
      (if (null curr)
          (progn  (setq curr trig)
                  (incf counter))
          counter))))

(defun block-scanner-c (trigger-string)
  (let* ((trig (coerce trigger-string 'list))
         (curr trig)
         (counter 0))
    (lambda (data-string)
      (loop :for c :across data-string
         :do (setq curr
                   (match curr
                    ((guard (list* a b) (char= a c)) b) ; next char
                    (nil (incf counter) trig)
                    (_   trig))))                       ; reset to trig
      (if (null curr)
          (progn (setq curr trig)
                 (incf counter))
          counter))))

;; maps are slower than loops
(defun block-scanner-c%% (trigger-string)
  (let* ((trig (coerce trigger-string 'list))
         (curr trig)
         (counter 0))
    (lambda (data-string)
       (map nil (lambda (c) (setq curr
                            (match curr
                              ((guard (list* a b) (char= a c)) b) ; next char
                              (nil (incf counter) trig)
                              (_   trig))))                       ; reset to trig
           data-string)
      (if (null curr)
          (progn (setq curr trig)
                 (incf counter))
          counter))))
