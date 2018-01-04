(defpackage #:reader
  (:nicknames #:r)
  (:documentation "provides the reader monad")
  (:use #:common-lisp)
  (:export :reader-state
           :reader-p
           :reader-run
           :ask
           :rpure
           :<*> :fmap :>>= :=<<))

(in-package :reader)


;; data Reader r a = Reader {runReader :: r -> a}
(defstruct (reader (:constructor make-reader (run)))
  (run #'identity :type function))


(defparameter ask (make-reader #'identity))

(defmethod fmap (f (m reader))
  (make-reader (lambda (x) (funcall f (funcall (reader-run m) x)))))


(defun rpure (x)
  (make-reader (constantly x)))

(defmethod <*> ((m reader) (k reader))
  (make-reader (lambda (x)
                 (let ((f (funcall (reader-run m) x))
                       (a (funcall (reader-run k) x)))
                   (funcall f a)))))


(defmethod >>= ((m reader) k)
  (make-reader (lambda (x)
                 (funcall (reader-run (funcall k (funcall (reader-run m) x)))
                          x))))

(defmethod =<< (k (m reader))
  (>>= m k))
