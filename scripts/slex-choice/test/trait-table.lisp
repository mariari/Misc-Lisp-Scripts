(in-package :slex-test)


(defmethod trait:name-of ((obj symbol))
  obj)

(defparameter *table-test* (trait:make-table))
(trait:insert *table-test*
              'yuki
              '(:cold :intrinsic)
              '(:bare-handed :ok))
