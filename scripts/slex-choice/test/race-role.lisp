(in-package :slex-test)

(def-suite race-role-test
    :description "Tests the race-role-map package")

(in-suite race-role-test)

(defparameter *test* (race-role:make-table))

(race-role:insert-race *test* 'yuki '(:bare-handed :ok) '(:cold :inrinsic))

(race-role:insert-role *test* 'undead-slayer '(:bare-handed :good))

(test test-race-insert
  (is (equalp (race-role:lookup-role-trait *test* :bare-handed)
              (list (trait:make-classification :name 'undead-slayer :qualifier :good)))))
