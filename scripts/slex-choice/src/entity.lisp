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

;;; ----------------------------------------------------------------------------
;;; Persistent exported data and types
;;; ----------------------------------------------------------------------------

(defparameter *synergy-map*      (race-role:make-table))
(defparameter *disadvantage-map* (race-role:make-table))

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
         `(defparameter ,astrix-name
            (make-instance 'entity
                           :name   ',name
                           :type   ,race/role
                           :traits (alist->hashtable ',without-desc (make-hash-table))
                           :description (or ,description ""))))))
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

;;; ----------------------------------------------------------------------------
;;; Helpers
;;; ----------------------------------------------------------------------------
(defun alist->hashtable (xss table)
  (dolist (item xss table)
    (setf (gethash (car item) table)
          (cadr item))))

(defun hashtable->string-keys (table)
  (subseq
   (with-output-to-string (builder)
     (maphash (lambda (key value) (format builder " :~A ~A" key value)) table))
   1))

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


