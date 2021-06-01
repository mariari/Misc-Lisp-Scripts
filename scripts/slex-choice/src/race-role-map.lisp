(defpackage #:scripts.slex.race-role-map
  (:documentation "provides a race role map structure")
  (:use #:cl)
  (:local-nicknames (:bi :scripts.slex.bi-table))
  (:export
   :make-table
   :insert-role
   :insert-race
   :lookup-roles-from-race
   :lookup-races-from-role))

(in-package :scripts.slex.race-role-map)

;; Internally we are backed by a bi-table

(defstruct table
  (race (bi:make-bi-table) :type bi:bi-table)
  (role (bi:make-bi-table) :type bi:bi-table))

(defun insert-race (table obj &rest traits)
  "inserts the object into the race table"
  (apply #'bi:insert (table-race table) obj traits)
  table)

(defun insert-role (table obj &rest traits)
  "inserts the object into the roll table"
  (apply #'bi:insert (table-role table) obj traits)
  table)

(flet ((lookup-generic (thing &key to from)
         (bi:lookup-from-traits
          to
          (bi:lookup-traits from thing))))
  (defun lookup-roles-from-race (table race)
    (lookup-generic race
                    :to   (table-role table)
                    :from (table-race table)))
  (defun lookup-races-from-role (table role)
    (lookup-generic role
                    :from (table-role table)
                    :to   (table-race table))))


