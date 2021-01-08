(defpackage #:scripts.mahjong
  (:documentation "Provides utility functions for the game of mahjong")
  (:use #:cl)
  (:export
   :calculate-score
   :round-up-ten
   :total-tsumo-score
   :tsumo-win
   :ron-win))

(in-package :scripts.mahjong)

;; total-tsumo-score ≥ ron
;; either it is slightly leser or exactly the same as the values get
;; higher


;; Note we don't add the riichi sticks here!

;; people normally use a scoring table, but we have lisp!
(defun ron-win (&key han (fu 20) eastp (honba 0))
  "Calculates the point you'd get by winning via ron"
  (let ((score (calculate-score :han han :fu fu)))
    (+ (* 300 honba)
       (round-up-to
        100
        (if eastp
            (* 6 score)
            (* 4 score))))))

(defun tsumo-win (&key han (fu 20) eastp (honba 0))
  "Calculates the point you'd get by each player from tsumo"
  (let* ((score          (calculate-score :han han :fu fu))
         (rounded        (round-up-to 100 score))
         (rounded-double (round-up-to 100 (* 2 score))))
    ;; don't bother with the tags
    (mapcar (lambda (x)
              (if (numberp x) (+ (* honba 100) x) x))
            (if eastp
                (list :others rounded-double)
                (list :others rounded :east rounded-double)))))

(defun total-tsumo-score (&key han (fu 20) eastp)
  (let ((payout (tsumo-win :han han :fu fu :eastp eastp)))
    (trivia:match payout
      ((list :others score) (* 3 score))
      ((list :others other :east east) (+ east (* 2 other))))))

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

(defun add-riichi-sticks (number)
  (* 1000 number))
