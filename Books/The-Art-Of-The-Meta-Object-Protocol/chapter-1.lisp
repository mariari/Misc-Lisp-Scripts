(in-package closette)

(defclass standard-class ()
  ((name :initarg :name
         :accessor class-name)
   (direct-superclasses :initarg :direct-superclasses
                        :accessor class-direct-superclasses)
   (class-precedence-list :accessor class-precedence-list)
   (effective-slots       :accessor class-slots)
   (direct-subclasses :initform ()
                      accessor class-direct-subclasses)
   (direct-methods :initform ()
                   :accessor class-direct-methods)))

(defmacro defclass (name direct-superclasses direct-slots &rest options)
  `(ensure-class ',name
                 :direct-superclasses ,(canonicalize-direct-superclasses
                                        direct-superclasses)
                 :direct-slots        ,(canonicalize-direct-slots
                                        direct-slots)
                 ,@(canonicalize-defclass-options options)))

(defun canonicalize-direct-slots (direct-slots)
  `(list ,@(mapcar #'canonicalize-direct-slot direct-slots)))

(defun canonicalize-direct-slot (spec)
  (if (symbolp spec)
      `(list :name ',spec)
      ))
