(defpackage #:scripts.slex.race-role-map
  (:documentation "provides a race role map structure")
  (:use #:cl)
  (:local-nicknames (:trait :scripts.slex.trait-table))
  (:export
   :make-table
   :insert-role
   :insert-race
   :lookup-role
   :lookup-role-trait
   :lookup-race
   :lookup-race-trait))

(in-package :scripts.slex.race-role-map)

;; backed by an trait table

(defstruct table
  (race (trait:make-table) :type trait:table)
  (role (trait:make-table) :type trait:table))

(defun insert-race (table obj &rest traits)
  "inserts the object into the race table"
  (apply #'trait:insert (table-race table) obj traits)
  table)

(defun insert-role (table obj &rest traits)
  "inserts the object into the roll table"
  (apply #'trait:insert (table-role table) obj traits)
  table)


(defun lookup-race (table obj)
  (trait:lookup-object (table-race table) obj))

(defun lookup-race-trait (table keyword)
  (trait:lookup-trait (table-race table) keyword))

(defun lookup-role (table obj)
  (trait:lookup-object (table-role table) obj))

(defun lookup-role-trait (table keyword)
  (trait:lookup-trait (table-role table) keyword))
