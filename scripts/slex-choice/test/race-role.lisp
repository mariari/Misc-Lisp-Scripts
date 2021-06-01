(in-package :slex-test)

(def-suite race-role-test
    :description "Tests the race-role-map package")

(in-suite race-role-test)

(defparameter *test* (race-role:make-table))

(race-role:insert-race *test* 'yuki 'bare-handed 'cold)

(race-role:insert-role *test* 'undead-slayer 'bare-handed)

(test test-race-insert
  (is (equalp (race-role:lookup-roles-from-race *test* 'yuki)
              '(UNDEAD-SLAYER))))
