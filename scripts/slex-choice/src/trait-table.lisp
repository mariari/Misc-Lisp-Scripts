(defpackage #:scripts.slex.trait-table
  (:documentation "Provides a trait symbol mapping")
  (:use #:cl)
  (:export
   :name-of
   :property
   ;; defstructs
   :table
   :table-p
   :make-table
   :table-trait
   :table-symbol
   :make-table
   :property
   :property-p
   :property-trait
   :property-qualifier
   :make-property
   :classification
   :classification-p
   :classification-name
   :classification-qualifier
   :make-classification
   ;; functions
   :insert
   :lookup-object
   :lookup-trait
   :modify-hash))

(in-package :scripts.slex.trait-table)

(defgeneric name-of (object)
  (:documentation "Grabs the name of the object handed to it"))


(defstruct table
  "the table is laid out such that the following relation is made

trait
(hash-table
  :bare-handed -> ((classification yuki :ok))
  :cold        -> ((yuki . :intrinsic)))

symbol
(hash-table
  yuki -> *yuki-object*)"
  (trait  (make-hash-table) :type hash-table)
  (symbol (make-hash-table) :type hash-table))

(defstruct property
  (trait     (error "fill this in") :type keyword)
  (qualifier (error "fill this in") :type keyword))

(defstruct classification
  (name      (error "fill this in"))
  (qualifier (error "fill this in") :type keyword))

(defun insert (table object &rest properties)
  "inserts an object into the table, this object must implement the name-of interface
the properties given can be either of type list or type classification"
  (dolist (property (mapcar #'list->property properties) table)
    (modify-hash (table-trait table) (property-trait property)
                 (lambda (classification-list)
                   (unique-insert
                    (make-classification :name      (name-of object)
                                         :qualifier (property-qualifier property))
                    classification-list)))
    (setf (gethash (name-of object) (table-symbol table))
          object)))


(defun lookup-object (table to-lookup)
  (gethash to-lookup (table-symbol table)))

(defun lookup-trait (table to-lookup)
  (gethash to-lookup (table-trait table)))

;;; ------------------------------------------------------
;;; Helpers
;;; ------------------------------------------------------
(defun list->property (list)
  "turns an alist into a property"
  (cond ((typep list 'list) (make-property :trait (car list) :qualifier (cadr list)))
        ((property-p list)  list)
        (t                  (error "can't convert to a property"))))

(defun modify-hash (hashtable to-find f)
  (setf (gethash to-find hashtable)
        (funcall f (gethash to-find hashtable))))

(defun unique-insert (x xs)
  (if (member x xs :test #'equalp)
      xs
      (cons x xs)))
