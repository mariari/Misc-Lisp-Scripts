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

(defmacro defrace (name &rest slot-groups)
  (let* ((astrix-name  (lol:symb (format nil "*~a*" (lol:mkstr name))))
         (grouped      (lol:group slot-groups 2))
         (description  (cadar (member :description grouped :key #'car)))
         (without-desc (remove-if (lambda (x) (equalp x :description)) grouped :key #'car)))
    `(defparameter ,astrix-name
       (make-instance 'entity
                      :name        ',name
                      :type        ,:race
                      :traits      (alist->hashtable ',without-desc (make-hash-table))
                      :description (or ,description "")))))


(defrace yuki-onna
  :description "They take a little extra damage whenever something damages them, but have extra melee power."
  :bare-handed :ok
  :cold        :inrinsic)

(defun alist->hashtable (xss table)
  (dolist (item xss table)
    (setf (gethash (car item) table)
          (cadr item))))

(defun hashtable->string-keys (table)
  (subseq
   (with-output-to-string (builder)
     (maphash (lambda (key value) (format builder " :~A ~A" key value)) table))
   1))


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

