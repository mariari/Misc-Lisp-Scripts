(defpackage #:scripts.mahjong
  (:documentation "Provides utility functions for the game of mahjong")
  (:use #:cl)
  (:export
   :calculate-score
   :round-up-ten))

(in-package :scripts.mahjong)

(defun calculate-score (&key han (fu 20))
  (cond
    ((>= han 13) 8000)
    ((>= han 11) 6000)
    ((>= han 8)  4000)
    ((>= han 6)  3000)
    ((>= han 5)  2000)
    (t           (min 2000 (* (round-up-ten fu) (expt 2 (+ 2 han)))))))

(defun round-up-ten (fu)
  (let ((moded (mod fu 10)))
    (if (zerop moded)
        fu
        (+ fu (- 10 (mod fu 10))))))
