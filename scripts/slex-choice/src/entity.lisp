(defpackage #:scripts.slex.entity
  (:documentation "provides entity for an entity structure")
  (:local-nicknames (:race-role :scripts.slex.race-role-map)
                    (:trait     :scripts.slex.trait-table))
  (:use #:cl)
  (:export
   :name :synergies :disadvantages :entity :insert
   :*entities*
   :defrace
   :defrole
   :hashtable->alist
   :lookup-entity
   :lookup-trait
   :other-race-role
   :race-role
   :traits))

(in-package :scripts.slex.entity)

;;; ----------------------------------------------------------------------------
;;; Persistent exported data and types
;;; ----------------------------------------------------------------------------
(defparameter *entities* (race-role:make-table))

(defclass entity ()
  ((name
    :initarg :name
    :accessor name
    :type symbol)
   (description
    :initarg :description
    :initform ""
    :accessor description
    :type string)
   (traits
    :initform (make-hash-table)
    :initarg :traits
    :accessor traits
    :type hash-table)
   (race-role
    :initform :role
    :initarg :type
    :accessor race-role
    :type keyword)))

(defmethod trait:name-of ((entity entity))
  (name entity))

(defmethod print-object ((obj entity) stream)
  (print-unreadable-object (obj stream :type nil)
    (format stream "~a ~a ~a"
            (race-role obj)
            (name obj)
            (hashtable->string-keys (traits obj)))))

(flet
    ((defgen (race/role name &rest slot-groups)
       (let* ((astrix-name  (lol:symb (format nil "*~a*" (lol:mkstr name))))
              (grouped      (lol:group slot-groups 2))
              (description  (cadar (member :description grouped :key #'car)))
              (without-desc
                  (remove-if (lambda (x) (equalp x :description)) grouped :key #'car)))
         `(prog1 (defparameter ,astrix-name
                   (make-instance 'entity
                                  :name   (intern (lol:mkstr ',name) :keyword)
                                  :type   ,race/role
                                  :traits (alist->hashtable ',without-desc (make-hash-table))
                                  :description (or ,description "")))
            (insert ,astrix-name)))))
  (defmacro defrace (name &rest slot-groups)
    "defines a Nethack race. May include a description and various
trait description associations
(defrace yuki-onna
  :description \"description of race here\"
  :bare-handed :ok
  :cold        :inrinsic)"
    (apply #'defgen :race name slot-groups))

  (defmacro defrole (name &rest slot-groups)
    "defines a Nethack role. May include a description and various
trait description associations
(defrole undead-slayer
  :description       \"Slow weak fighter whose…\"
  :bare-handed       :grand-master
  :bare-handed-style :marital-arts
  :flavor            \"Undead Slayers are specialists, …\")"
    (apply #'defgen :role name slot-groups)))

(defun insert (entity &optional (*entities* *entities*))
  (with-slots ((type  race-role)
               (traits traits))
      entity
    (labels ((add (function)
               (apply function *entities* entity (hashtable->alist traits))))
      (cond ((eql type :race)
             (add #'race-role:insert-race))
            ((eql type :role)
             (add #'race-role:insert-role))
            (t (error "malformed entity"))))))

(defun other-race-role (keyword)
  (if (eq :race keyword)
      :role
      :race))

(defun lookup-trait (trait race-role)
  "we lookup the trait on the race-role-map specified"
  (if (eq :race race-role)
      (race-role:lookup-race-trait *entities* trait)
      (race-role:lookup-role-trait *entities* trait)))

(defun lookup-entity (name)
  "looks up the entity in the map returning the race and/or role. giving
back both if the name belongs to both a race and a role"
  (let ((try-race (race-role:lookup-race *entities* name))
        (try-role (race-role:lookup-role *entities* name)))
    (if (and try-race try-role)
        (values try-race try-role)
        (or try-race try-role))))

;;; ----------------------------------------------------------------------------
;;; Helpers
;;; ----------------------------------------------------------------------------
(defun alist->hashtable (xss table)
  (dolist (item xss table)
    (setf (gethash (car item) table)
          (cadr item))))

(defun hashtable->alist (table)
  (loop :for key   :being :the :hash-keys   :of table
        :for value :being :the :hash-values :of table
        :collect (list key value)))

(defun hashtable->string-keys (table)
  (subseq
   (with-output-to-string (builder)
     (maphash (lambda (key value) (format builder " :~A ~A" key value)) table))
   1))
