(defpackage #:scripts.slex.queries
  (:documentation "Querying for entities applying rules when applicable")
  (:local-nicknames (:race-role :scripts.slex.race-role-map)
                    (:trait     :scripts.slex.trait-table)
                    (:entity    :scripts.slex.entity)
                    (:rules     :scripts.slex.rules))
  (:use #:cl)
  (:export :lookup-synergies))

(in-package :scripts.slex.queries)

(defun lookup-synergies (entity &key (*entities* entity:*entities*) (as-good-as 0))
  "looks up a sorted synergetic list of races/roles that go with the
current entity"
  (with-slots ((type   entity:race-role)
               (traits entity:traits))
      entity
    (let ((as-good-as (if (keywordp as-good-as)
                          (rules:lookup-value as-good-as 0)
                          as-good-as)))
      (mapcar (lambda (trait-property)
                (if (rules:synergetic-traitp (car trait-property) (cadr trait-property))
                    (rules:lookup-and-score
                     (entity:lookup-trait (car trait-property)
                                          (entity:other-race-role type)))
                    '()))
              (entity:hashtable->alist traits)))))

;; (lookup-synergies (entity:lookup-entity :undead-slayer))
;; (((:YUKI-ONNA 3)) NIL NIL)
;; (lookup-synergies (entity:lookup-entity :yuki-onna))
;; (((:UNDEAD-SLAYER 9)) NIL)
