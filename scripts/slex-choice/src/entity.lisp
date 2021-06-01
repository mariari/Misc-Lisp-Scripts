(defpackage #:scripts.slex.entity
  (:documentation "provides entity for an entity structure")
  (:local-nicknames (:race-role :scripts.slex.race-role-map))
  (:use #:cl)
  (:export
   :name :synergies :disadvantages :entity :insert
   :*synergy-map* :*disadvantage-map*
   :defrace
   :defrole))

(in-package :scripts.slex.entity)

(defclass entity ()
  ((name
    :initarg :name
    :accessor name
    :type symbol)
   (synergies
    :initform nil
    :initarg :synergies
    :accessor synergies
    :type list)
   (disadvantages
    :initform nil
    :initarg :disadvantages
    :accessor disadvantages
    :type list)
   (race-role
    :initform :role
    :initarg :type
    :accessor race-role
    :type keyword)))

(defmethod print-object ((obj entity) stream)
  (print-unreadable-object (obj stream :type nil)
    (format stream "~a ~a :SYNERGIES ~a :DISADVANTAGES ~a"
            (race-role obj)
            (name obj)
            (synergies obj)
            (disadvantages obj))))

(defmacro defrace (name &key syn dis)
  "generates a race with the given name expanding to *name*"
  (let ((astrix-name (lol:symb (format nil "*~a*" (lol:mkstr name)))))
    `(defparameter ,astrix-name
       (make-instance 'entity
                       :type ,:race
                       :name ',name
                       :synergies ,syn
                       :disadvantages ,dis))))

(defmacro defrole (name &key syn dis)
  "generates a race with the given name expanding to *name*"
  (let ((astrix-name (lol:symb (format nil "*~a*" (lol:mkstr name)))))
    `(defparameter ,astrix-name
       (make-instance 'entity
                      :name ',name
                      :synergies ,syn
                      :disadvantages ,dis))))


(defparameter *synergy-map*      (race-role:make-table))
(defparameter *disadvantage-map* (race-role:make-table))

(defun insert (entity)
  (flet ((add (f)
           (list
            (apply f *synergy-map*      entity (synergies entity))
            (apply f *disadvantage-map* entity (disadvantages entity)))))
    (with-slots ((type race-role)) entity
      (cond ((eql type :race)
             (add #'race-role:insert-race))
            ((eql type :role)
             (add #'race-role:insert-role))
            (t (error "malfromed entity"))))))

