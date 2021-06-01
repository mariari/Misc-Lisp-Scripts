(defpackage #:scripts.slex.bi-table
  (:documentation "Provides a bi-table for looking up information forwards and
backwards")
  (:use #:cl)
  (:export
   :make-bi-table
   :bi-table-backwards
   :bi-table-forwards
   :lookup-traits
   :lookup-corresponding
   :lookup-backwards
   :insert
   :lookup-from-traits
   :bi-table))

(in-package :scripts.slex.bi-table)

;;; Type mapping
;;; ∀ symb traits.
;;; forward  : symb   -> list traits
;;; backward : traits -> list symb

(defstruct bi-table
  (forwards  (make-hash-table) :type hash-table)
  (backwards (make-hash-table) :type hash-table))

(defun lookup-traits (table symbol)
  "lookups the traits from a given input"
  (gethash symbol (bi-table-forwards table)))

(defun lookup-corresponding (table symbol)
  "lookups the corresponding elements we care about given a symb"
  ;; can't use mapcan, as it nconcs, which mutates some reference...
  (remove-if (lambda (ele) (eql ele symbol))
             (lookup-from-traits
              table
              (gethash symbol (bi-table-forwards table)))))

(defun lookup-from-traits (table traits)
  "gives a list of all objects that satisfy the trait"
  ;; execuse cause we don't use a set!
  (remove-duplicates
   (apply #'append
          (mapcar (lambda (trait)
                    (lookup-backwards table trait))
                  traits))))

(defun lookup-backwards (table trait)
  "lookups the given thing we care about given the trait"
  (gethash trait (bi-table-backwards table)))

(defun insert (table symbol &rest traits)
  "inserts a symbol"
  (setf (gethash symbol (bi-table-forwards table)) traits)
  (mapc (lambda (trait)
          (let ((symbols (gethash trait (bi-table-backwards table))))
            (setf (gethash trait (bi-table-backwards table))
                  (append
                   ;; sets make this much faster
                   (unless (member symbol symbols) (list symbol))
                   symbols))))
        traits)
  table)
