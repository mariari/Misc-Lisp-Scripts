(defpackage #:rmv2
  (:documentation "A resource machine implementation and exploration")
  (:shadow :@)
  (:use #:common-lisp #:serapeum))

(in-package #:rmv2)

;; We are mostly ignoring details about references for now
(defclass resource ()
  ((label :initarg :label :accessor label :type list)
   (data  :initarg :data  :accessor data :type list)
   ;; Think of a better init form for the compilation
   (logic :initarg :logic
          :accessor logic
          :initform t)
   (quantity :accessor quantity
             :initarg :quantity
             :type integer
             :initform 1)
   (nonce :accessor nonce
          :initarg :nonce
          :type (simple-vector 256)
          :initform (make-array 256 :element-type 'unsigned-byte))
   (ephemeral :initarg :ephmeral-p
              :accessor ephemeral-p
              :initform nil
              :type boolean
              :documentation "I represent if a resource is ephmeral")
   (nullifier-key-commitment
    :accessor nullifier-commitment
    :initarg :nullifier-commitment
    :initform nil)
   (randseed :accessor randseed
             :initarg :randseed
             :type integer
             :initform 0))
  (:documentation "I am a resource"))

;; I can form the rest as well though the exact machinery around
;; forming actions is more annoying to do properly

;; Our instance is a rough approximation of a proving context for an
;; individual resource... This not completely accurate
(defclass instance ()
  ((tag :initarg :tag :accessor tag)
   (consumed-p :initarg :consumed-p :accessor consumed-p :type boolean :initform t)
   (consumed   :initarg :consumed :accessor consumed :type list :initform nil)
   (created    :initarg :created  :accessor created  :type list :initform nil)))


(defclass compliance-unit ()
  ((proof    :initarg :proof :accessor proof :initform nil)
   (vk       :initarg :verifying-key :accessor verifying-key :initform nil)
   (instances :initarg :instances :accessor instances :initform nil))
  (:documentation "In a transparent case the only real field is instance"))

(-> make-compliance-unit (list) compliance-unit)
(defun make-compliance-unit (instances)
  (values (make-instance 'compliance-unit :instances instances)))

;; Instead let us model our own view of objects into a resource Let us
;; split the following, we have some methods that compile to an
;; object, this goes into the obj-resource-logic
;; Next, we define out the relation

(defgeneric obj-resource-logic (object instance any)
  (:documentation "I run the resource obj-resource-logic for a given type"))

;; We need our mapping to a resource
(defmethod obj->resource ((x standard-object))
  (let ((class (class-of x)))
    (make-instance
     'resource
     :data (instance-values x)
     :logic #'obj-resource-logic
     ;; Label is a reference to the slot values that we need to refer
     ;; to, however since we recreate the value and the data is
     ;; available Ill elide this detail, what we want is to basically
     ;; have a reference to the specific class values in some way
     ;; which can change, not sure what this looks like. However we
     ;; can always retrieve the class this way
     :label class)))

(defmethod obj->resource ((x number))
  (make-instance 'resource :data (list x)
                           :logic #'obj-resource-logic
                           :label 'built-in-class))

(-> verify (resource instance t) boolean)
(defun verify (resource instance any)
  ;; time for the fun
  (assure boolean
    (funcall (logic resource)
             (resource->obj resource)
             instance
             any)))

(-> resource->obj (resource) t)
(defun resource->obj (x)
  (case (label x)
    (built-in-class
     (car (data x)))
    (t
     (list-to-class (c2mop:ensure-finalized (label x))
                    (data x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example Resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass integer-obj (pointwise-mixin)
  ((data :initarg :data :accessor data :type integer)))

;; We can do this via a meta class but I want to see how this works
(defclass counted-integer (integer-obj)
  ((counter :initarg :counter
            :accessor counter
            :allocation :class
            :initform 0)))

(-> mk-integer (integer) integer-obj)
(defun mk-integer (x)
  (values (make-instance 'integer-obj :data x)))

(-> counted (integer) counted-integer)
(defun counted (x)
  (values (make-instance 'counted-integer :data x)))


(defmethod obj-resource-logic ((object integer-obj) (instance instance) any)
  ;; We can ignore the instance as our constraint is very simple
  (integerp (data object)))

(defmethod obj-resource-logic ((object integer) (instance instance) any)
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods as resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This part gets tricky as we have to define out a few kinds of
;; resources

;; 1. an order of operations resource, this is an expensive way to
;; ensure argumentation order, for now we will fix inputs and outputs
;; Artem mentions I can use the order of values for input and output
;; checking

;; 2. the operational check itself, many of these will be trivial, but
;; we do have to make sure their own invariants hold
;; J: Does this mean we can simply check the output is the expected
;; value?


(defclass method-resource ()
  ;; type is funcallable to be more accurate.
  ;; If we have more outputs, then we'd care about that as a field
  ((gf :initarg :gf :accessor gf :type symbol)))

(defmethod obj-resource-logic ((object method-resource) (instance instance) any)
  (obj-equalp
   ;; We check the output is equal to the work
   (resource->obj (cadr (created instance)))
   (apply (gf object)
          (mapcar #'resource->obj (consumed instance)))))

(defun add (arg1 &rest argn)
  (reduce #'add-2 argn :initial-value arg1))

;; Let us make our first operation
(defmethod add-2 ((int1 integer-obj)
                  (int2 integer-obj))
  ;; if something is a subclass make that instead
  (make-instance (class-of int1)
                 :data (+ (data int1)
                          (data int2))))

;; RMV2> (add (counted 6) (counted 5) (mk-integer 12))

;; We can verify this in an interesting way
;; 1. eval then package it into a resource
;; 2. leave it open to solve as an intent bearing transaction

(defmacro transact (expression)
  `(transact-expression (list ',(car expression)
                              ,@(cdr expression))
                        ,expression))

(defun transact-expression (expression result)
  (let ((consumed (mapcar #'obj->resource (cdr expression)))
        (output   (obj->resource result))
        ;; just using the car isn't the most elegant
        (function (obj->resource
                   (make-instance 'method-resource :gf (car expression)))))
    (labels ((create-consumed (object)
               (make-instance 'instance
                              :created (list function output)
                              :consumed consumed
                              :consumed-p t
                              ;; modeling of tag not online
                              :tag object))
             (create-output (object)
               (let ((obj (create-consumed object)))
                 (setf (consumed-p obj) nil)
                 obj)))
      (make-compliance-unit
       ;; Order is: function, output, inputs
       (list* (create-output function)
              (create-output output)
              (mapcar #'create-consumed consumed))))))

;; Now let us verify our compliance unit
(-> verify-compliance-unit (compliance-unit) boolean)
(defun verify-compliance-unit (compliance)
  (let ((instances (instances compliance)))
    (every (lambda (instance)
             (verify (tag instance) instance t))
           instances)))

;; (defvar *known-verification-functions* (make-hash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example Invocations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RMV2> (verify (obj->resource (counted 4)) (make-instance 'instance) 3)
;; T
;; RMV2> (defparameter *x* (obj->resource (counted 4)))
;; *X*
;; RMV2> (setf (data *x*) (list "hi"))
;; ("hi")
;; RMV2> (verify *x* (make-instance 'instance) 3)
;; NIL

;; RMV2>
;; (verify-compliance-unit
;;  (transact (add (counted 6)
;;                 (add (counted 20) (counted 122))
;;                 (counted 5)
;;                 (mk-integer 12))))
;; T

;; RMV2>
;; (defparameter *transaction* (transact (add (counted 6)
;;                                            (add (counted 20) (counted 122))
;;                                            (counted 5)
;;                                            (mk-integer 12))))
;; *TRANSACTION*
;; RMV2> (defparameter *add* (car (instances *transaction*)))
;; *ADD*

;; RMV2> (setf (consumed *add*) (cons (car (consumed *add*)) (consumed *add*)))

;; RMV2> (verify-compliance-unit *transaction*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generalized helpers
;;
;; Almost all of this code was stolen from GEB, Alucard, or my first
;; RM implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symbol-to-keyword (symbol)
  "Turns a [symbol] into a [keyword]"
  (intern (symbol-name symbol) :keyword))

(defmacro define-generic-print (type)
  `(defmethod print-object ((obj ,type) stream)
     (pprint-logical-block (stream nil)
       (print-unreadable-object (obj stream :type t)
         (format stream "~2I~{~_~A~^ ~}" (to-list obj))))))

(define-generic-print resource)
(define-generic-print instance)
(define-generic-print compliance-unit)
(define-generic-print integer-obj)
(define-generic-print counted-integer)

(defun list-to-class (class list)
  ;; Should we make a prototype or do an instance like this I'm
  ;; skeptical of an instance due to finalization logic that we should
  ;; not go through.

  ;; For now with no docs, we shall do this, but when I have internet
  ;; do something better
  (apply #'make-instance
         class
         (mapcan (lambda (slot value)
                   (list (car (c2mop:slot-definition-initargs slot)) value))
                 (instance-slots class)
                 list)))

(defgeneric to-list (object)
  (:documentation "Turns an object into a list")
  (:method ((object standard-object))
    (mapcar #'cdr (to-pointwise-list object))))

;; Bad way to define these but w/e
(defgeneric instance-values (object)
  (:documentation "Gets all the instance values of a list")
  (:method ((object standard-object))
    (mapcar (lambda (x) (slot-value object x))
            (mapcar #'c2mop:slot-definition-name
                    (instance-slots (class-of object))))))

(defgeneric class-values (object)
  (:documentation "Gets all the class values of a list")
  (:method ((object standard-object))
    (mapcar (lambda (x) (slot-value object x))
            (mapcar #'c2mop:slot-definition-name
                    (remove-if-not
                     (lambda (x) (eq :class (c2mop:slot-definition-allocation x)))
                     (pointwise-slots object))))))

(defun instance-slots (class)
  (remove-if-not (lambda (x)
                   (eq :instance
                       (c2mop:slot-definition-allocation x)))
                 (c2mop:compute-slots class)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pointwise Mixins
;; These just control equality and how slots are computed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass pointwise-mixin () ()
  (:documentation "Provides the service of giving point wise
                   operations to classes"))

(defclass direct-pointwise-mixin (pointwise-mixin) ()
  (:documentation "Works like POINTWISE-MIXIN, however functions on
                   [POINTWISE-MIXIN] will only operate on direct-slots
                   instead of all slots the class may contain.

                   Further all `DIRECT-POINTWISE-MIXIN`'s are [POINTWISE-MIXIN]'s"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; API for Pointwise
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; my way of coping with no meta classes by default
(defgeneric pointwise-slots (obj)
  (:documentation "Works like C2MOP:COMPUTE-SLOTS however on the object
                   rather than the class")
  ;; should we specialize it on pointwise-mixin instead? Should all
  ;; objects be able to give their pointwise slots?
  (:method ((object standard-object))
    (c2mop:compute-slots (class-of object))))

(defgeneric obj-equalp (object1 object2)
  (:documentation "Compares objects with pointwise equality. This is a
                   much weaker form of equality comparison than
                   STANDARD-OBJECT EQUALP, which does the much
                   stronger pointer quality")
  (:method ((obj1 standard-object) (obj2 standard-object))
    "for non pointwise objects, compute the standard equalp"
    (equalp obj1 obj2)))

(defgeneric to-pointwise-list (obj)
  (:documentation "Turns a given object into a pointwise LIST. listing
                   the KEYWORD slot-name next to their value.")
  (:method ((obj standard-object))
    (mapcar (lambda (x)
              (cons (symbol-to-keyword x)
                    (slot-value obj x)))
            (mapcar #'c2mop:slot-definition-name
                    (pointwise-slots obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instances
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod pointwise-slots ((object direct-pointwise-mixin))
  "Works like the normal POINTWISE-SLOTS however we only work on
   direct slot values"
  (c2mop:class-direct-slots (class-of object)))

(defmethod obj-equalp ((obj1 pointwise-mixin) (obj2 pointwise-mixin))
  (and (c2mop:subclassp (type-of obj1) (type-of obj2))
       (obj-equalp (to-pointwise-list obj1)
                   (to-pointwise-list obj2))))

(defmethod obj-equalp ((obj1 list) (obj2 list))
  (or (eq obj1 obj2)
      (and (consp obj1)
           (consp obj2)
           (obj-equalp (car obj1) (car obj2))
           (obj-equalp (cdr obj1) (cdr obj2)))))

;; I should implement it for arrays as well!
(defmethod obj-equalp ((obj1 t) (obj2 t))
  (equalp obj1 obj2))
