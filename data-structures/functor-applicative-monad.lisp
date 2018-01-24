(defpackage #:generic
  (:nicknames #:g)
  (:documentation "provides generic functions, fn* denotes the generic function that takes n things")
  (:use #:common-lisp)
  (:export :<$
           :liftA2
           :<* :*>
           :>>
           :>>* :>>=* :=<<* :*>* :<**
           :<*> :fmap :>>= :=<<))

(in-package :generic)
;; functor
(defgeneric fmap (f xs)
  (:documentation "functor map"))

(defun <$ (a xs)
  (fmap (constantly a) xs))

;; Applicative
(defgeneric <*> (f xs)
  (:documentation "the applicative function"))

;; can't make this generically with CL's type system
;; (defgeneric pure (x)
;;   (:documentation "lifts a value into the applicative structure"))
(defun liftA2 (f xs ys)
  (<*> (fmap f xs) ys))


(defun *> (a1 a2)
  (<*> (<$ #'identity a1) a2))

(defun <* (a1 a2)
  (liftA2 #'constantly a1 a2))

;; alternative definition
;; (defun <* (a1 a2)
;;   (<*> (fmap #'constantly a1) a2))


;; monad
(defgeneric >>= (xs f)
  (:documentation "the monadic bind function"))

(defgeneric =<< (f xs)
  (:documentation "the reverse bind function"))

;; can't make this generically with CL's type system
;; (defgeneric return (x)
;;   (:documentation "lifts a value into the monadic structure"))

(defun >> (f g)
  (>>= f (constantly g)))

;; n-ary versions

(defun >>* (&rest fns)
  (reduce #'>> fns :from-end t))

(defun >>=* (&rest fns)
  (reduce #'>>= fns :from-end t))

(defun =<<* (&rest fns)
  (reduce #'=<< fns :from-end t))

(defun *>* (&rest fns)
  (reduce #'*> fns :from-end t))

(defun <** (&rest fns)
  (reduce #'<* fns :from-end t))