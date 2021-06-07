(in-package :slex-test)


(defparameter *table-test* (trait:make-table))
(trait:insert *table-test*
              'yuki
              '(:cold :intrinsic)
              '(:bare-handed :ok))
