(in-package :slex-test)

(def-suite entity-test
    :description "Tests the entity package")

(in-suite entity-test)


;; (entity:defrace yuki-onna :syn '(bare-handed cold))

;; (entity:defrole undead-slayer :syn '(bare-handed))

(test test-entity-insertion
  ;; (is
  ;;  (let ((entity:*synergy-map*      (race-role:make-table))
  ;;        (entity:*disadvantage-map* (race-role:make-table)))
  ;;    (entity:insert *yuki-onna*)
  ;;    (entity:insert *undead-slayer*)
  ;;    (equalp
  ;;     (race-role:lookup-roles-from-race entity:*synergy-map* *yuki-onna*)
  ;;     (list *undead-slayer*))))
  )
