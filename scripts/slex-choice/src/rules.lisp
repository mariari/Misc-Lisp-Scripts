(defpackage #:scripts.slex.rules
  (:documentation "provides rule for comparing symbols, and rules of the slex game")
  (:local-nicknames (:race-role :scripts.slex.race-role-map)
                    (:trait :scripts.slex.trait-table)
                    (:entity    :scripts.slex.entity))
  (:use #:cl)
  (:export
   :value
   :defvalue
   :*value-table*
   :defrule
   :lookup-value
   :synergetic-traitp
   :lookup-and-score))

(in-package :scripts.slex.rules)


(defgeneric value (object)
  (:documentation "gives the value of the current keyword like (:ok :good :great …) to a
number value to make a hierarchy of values"))

(defparameter *value-table* (make-hash-table :test #'eq))

(defun defvalue (qualifier value)
  "defines a qualifier with a value in the values table. Currently these are stored as numbers
(defvalue :ok 3)"
  (setf (gethash qualifier *value-table*) value))

(defun lookup-value (qualifier &optional default)
  "looks up the value of the given qualifier from the value table. must
be an eq-able value. The default value is given if it is not found.
(lookup-value :ok) => 3"
  (gethash qualifier *value-table* default))

(defun synergetic-traitp (trait skill)
  "determines if the trait is synergetic or not"
  t)

(defun lookup-and-score (classification-list)
  "we lookup the score and the name of the entity in the classification
list."
  (mapcar (lambda (classifcation)
            (list
             (trait:classification-name classifcation)
             (lookup-value (trait:classification-qualifier classifcation)
                           0)))
          classification-list))

(defmacro defrule (&rest rest)
  `(quote ,rest))
