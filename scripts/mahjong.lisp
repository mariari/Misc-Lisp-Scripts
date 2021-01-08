(defpackage #:scripts.mahjong
  (:documentation "Provides utility functions for the game of mahjong")
  (:use #:cl)
  (:export
   :calculate-score
   :round-up-ten
   :total-tsumo-score
   :ron-win))

(in-package :scripts.mahjong)

(defun ron-win (&key han (fu 20) eastp)
  (let ((score (calculate-score :han han :fu fu)))
    (round-up-to
     100
     (if eastp
         (* 6 score)
         (* 4 score)))))

(defun total-tsumo-score (&key han (fu 20) eastp)
  (let ((payout (tsumo-win :han han :fu fu :eastp eastp)))
    (trivia:match payout
      ((list :others score) (* 3 score))
      ((list :others other :east east) (+ east (* 2 other))))))

(defun tsumo-win (&key han (fu 20) eastp)
  (let* ((score          (calculate-score :han han :fu fu))
         (rounded        (round-up-to 100 score))
         (rounded-double (round-up-to 100 (* 2 score))))
    (if eastp
        (list :others rounded-double)
        (list :others rounded :east rounded-double))))

(defun calculate-score (&key han (fu 20))
  (cond
    ((>= han 13) 8000)
    ((>= han 11) 6000)
    ((>= han 8)  4000)
    ((>= han 6)  3000)
    ((>= han 5)  2000)
    (t           (min 2000 (* (round-up-to 10 fu) (expt 2 (+ 2 han)))))))

(defun round-up-10 (fu)
  (round-up-to 10 fu))

(defun round-up-to (base fu)
  (let ((moded (mod fu base)))
    (if (zerop moded)
        fu
        (+ fu (- base (mod fu base))))))
