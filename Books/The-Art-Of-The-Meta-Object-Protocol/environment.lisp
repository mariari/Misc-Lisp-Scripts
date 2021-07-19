(defpackage closette
  ;; have to do this as we start shadowing CLOS
  (:import-from :cl
                defpackage in-package + nil list cons mapcar defstruct
                car cdr cons eq not and or defmacro &rest &body &optional
                defun symbolp if)
  (:export))

(in-package closette)
