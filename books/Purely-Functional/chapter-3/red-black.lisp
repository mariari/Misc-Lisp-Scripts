(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:trivia
                  :fset))
  (rename-package 'fset 'fset '(:f))
  (use-package 'trivia))

;; Red and Black tree**********************************************************************************************
(defconstant +red+ 'red)
(defconstant +black+ 'black)

(deftype color ()
  "colors for a red-black tree"
  `(member ,+red+ ,+black+))

