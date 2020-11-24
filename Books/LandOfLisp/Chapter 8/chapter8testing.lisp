;; Testing------------
(loop repeat 10
   collect 1)

(loop for n from 1 to 10
   collect n)

(loop for n from 1 to 10
   collect (+ 100 n))
