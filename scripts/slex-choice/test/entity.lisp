(in-package :slex-test)

(def-suite entity-test
    :description "Tests the entity package")

(in-suite entity-test)

(defparameter *testing-entity-map* (race-role:make-table))

(let ((entity:*entities* *testing-entity-map*))
  (entity:defrace yuki-onna
    :description "They take a little extra damage whenever something damages them, but have extra melee power."
    :bare-handed :ok
    :cold        :inrinsic)

  (entity:defrole undead-slayer
    :description "Slow, weak fighter whose main advantages (drain resistance and immunity to sickness) are less important in early game than late game"
    :bare-handed       :grand-master
    :bare-handed-style :marital-arts
    :flavor "Undead Slayers are specialists, trained to hunt the undead as well as other incarnations of evil. They are well aware of the weaknesses of their foes and come prepared. Few denizens of darkness ever encounter such warriors of light and live to tell of it."))


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
