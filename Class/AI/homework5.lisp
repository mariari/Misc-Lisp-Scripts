(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:eazy-gnuplot :clml.statistics))
  (use-package :eazy-gnuplot)
  (rename-package 'clml.statistics 'clml.statistics '(:s)))


(ensure-directories-exist "images/")

(defmacro curry (fn . args)
  "Creates a partially applied function that takes 1 argument"
  (let ((arg (gensym)))
    `(lambda (,arg) (,fn ,@args ,arg))))`


(defun range (max &optional (min 0) (step 1))
  (loop for x from min to max by step
     collect x))
(defun fact (num &optional (bot 1))
  (reduce #'* (range num bot)))

(defun choose (num group)
  (/ (fact num (1+ (- num group))) (fact group)))


(defun binomial-plot (output number)
  (with-plots (*standard-output* :debug nil)
    (gp-setup :terminal '(pngcairo)
              :output output)
    (plot (lambda ()
            (map 'nil
                 (lambda (x y) (format t "~&~a ~a" x y))
                 (f:range 0 1 (/ 1.0 number))
                 (generate-binomial-graph .2 number 100000)))
          :with '(:line)))
  output)

(defun generate-binomial-graph (p number sample)
  (let* ((dist (s:binomial-distribution 1 p))
         (data (mapcar (lambda (x) (apply #'+ x))
                       (loop for i from 1 to sample collect (s:rand-n dist number))))
         (arr (make-array number :initial-element 0)))
    (mapc (lambda (x) (incf (elt arr x))) data)
    arr))

;; (binomial-plot "images/binomial-plot.png"  20)


;;; Questions------------------------------------------------------------------------------------------------

;; number b
(defun equation (n θ y)
  (* (choose n y) (expt θ y) (expt (- 1 θ) (- n y))))

(defun derived (n θ y)
  (* (1+ n) (equation n θ y)))


(defun generate-1b-plot (output)
  (with-plots (*standard-output* :debug t)
    (gp-setup :terminal '(pngcairo :size |35cm,35cm|)
              :output output
              :ylabel "probabilty"
              ;; :xrange '("[0:5]")
              :xlabel "number of heads"
              :pointsize "3px"
              :title "Plot the likelihood for n = 4 and θ = 3/4. Make sure your plot includes y = 0."
              :style '(fill pattern 5 border))
    (plot (lambda ()
            (format t "-1 0")
            (mapcar (lambda (y) (format t "~&~a ~a" y (* 1.0 (equation 4 3/4 y))))
                    (f:range 0 5)))
          :using (list "1:2 with boxes") :title "likelihood"))
  output)

(generate-1b-plot "images/1b-plot.png")

(defun generate-1c-plot-gen (output n y str)
  (with-plots (*standard-output* :debug nil)
    (gp-setup :terminal '(pngcairo :size |35cm,20cm|)
              :output output
              :title str
              :xlabel "value of θ"
              :ylabel "posterior of θ")
    (plot (lambda ()
            (mapcar (lambda (θ) (format t "~&~a ~a" (* 1.0 θ) (* 1.0 (derived n θ y))))
                    (f:range 0 1 1/1000)))
     :with '(:line) :title ""))
  output)

(defun generate-1c-plot-1 (output)
  (generate-1c-plot-gen output 1 1 "1 head 0 tails"))

(defun generate-1c-plot-2 (output)
  (generate-1c-plot-gen output 2 2 "2 heads 0 tails"))

(defun generate-1c-plot-3 (output)
  (generate-1c-plot-gen output 3 2 "2 heads 1 tail"))

(defun generate-1c-plot-4 (output)
  (generate-1c-plot-gen output 4 3 "3 heads 1 tail"))


(generate-1c-plot-1 "images/1c-1.png")
(generate-1c-plot-2 "images/1c-2.png")
(generate-1c-plot-3 "images/1c-3.png")
(generate-1c-plot-4 "images/1c-4.png")


;; Problem 2



;; 0 is cherry
;; 1 is lime
(defun bags (hi)
  (case hi (1 1) (2 .75) (3 .5) (4 .25) (5 0)))

(defun bag1 () 0)
(defun bag2 () (if (= 0 (random 4)) 1 0))
(defun bag3 ()
  (let ((rand (random 4)))
    (if (or (= rand 1) (= rand 0))
        1 0)))
(defun bag4 () (if (= 0 (random 4)) 0 1))
(defun bag5 () 1)


(defun suffixes (xs)
  (if (null xs)
      nil
      (cons xs (suffixes (cdr xs)))))

(defun d-given-hi (d hi)
  (apply #'* (mapcar (lambda (x) (if (zerop x) (bags hi) (- 1 (bags hi)))) d)))

(defun normalize (d)
  (/ 1.0 (apply #'+ (mapcar (lambda (hi) (d-given-hi d hi)) (f:range 1 5)))))


(defun generate-2a-plot (output bag)
  (with-plots (*standard-output* :debug nil)
    (gp-setup :terminal '(pngcairo :size |35cm,20cm|)
              :output output
              :xlabel "Number of observations in d"
              :ylabel "Posterior probability of hypothesis")
    (let ((drawn (reverse (suffixes (loop for i from 0 to 100 collect (funcall bag))))))
      (macrolet ((equation (num)
                   `(plot (lambda ()
                            (format t "~&~a ~a" 0 0)
                            (mapcar (lambda (n di) (format t "~&~a ~a" n (* (normalize di) (d-given-hi di ,num))))
                                    (f:range 1 100)
                                    drawn))
                     :with '(:linespoint :title ,(concatenate 'string "P(h" (princ-to-string num) " | d)")))))
        (equation 1) (equation 2) (equation 3) (equation 4) (equation 5))))
  output)

(generate-2a-plot "images/p-2-h1.png" #'bag1)
(generate-2a-plot "images/p-2-h2.png" #'bag2)
(generate-2a-plot "images/p-2-h3.png" #'bag3)
(generate-2a-plot "images/p-2-h4.png" #'bag4)
(generate-2a-plot "images/p-2-h5.png" #'bag5)

(defun probs-next-candy (d)
  (apply #'+ (mapcar (lambda (num)
                       (* (normalize d) (d-given-hi d num) (- 1 (bags num))))
                     (f:range 1 5))))


(defun generate-2d-plot (output bag title)
  (with-plots (*standard-output* :debug nil)
    (gp-setup :terminal '(pngcairo :size |35cm,20cm|)
              :output output
              :title title
              :xlabel "Probability that next candy is lime"
              :ylabel "Number of observations in d")
    (gp :unset :key)
    (let ((drawn (reverse (suffixes (loop for i from 0 to 100 collect (funcall bag))))))
      (plot (lambda ()
              (format t "~&~a ~a" 0 0)
              (mapcar (lambda (n di) (format t "~&~a ~a" n (probs-next-candy di)))
                      (f:range 1 100)
                      drawn))
            :with '(:linespoint))))
  output)

(generate-2d-plot "images/p-2-d1.png" #'bag1 "")
(generate-2d-plot "images/p-2-d2.png" #'bag2 "")
(generate-2d-plot "images/p-2-d3.png" #'bag3 "")
(generate-2d-plot "images/p-2-d4.png" #'bag4 "")
(generate-2d-plot "images/p-2-d5.png" #'bag5 "")


(defun average (list)
  (/ (apply #'+ list) (length list)))

(defun generate-2c-plot (output bag num title)
  (with-plots (*standard-output* :debug nil)
    (gp-setup :terminal '(pngcairo :size |35cm,20cm|)
              :output output
              :title title
              :xlabel "Number of observations in d"
              :ylabel "error from bag distribution")
    (flet ((collect () (reverse (suffixes (loop for i from 0 to 100 collect (funcall bag))))))
      (plot (lambda ()
              (mapcar (lambda (n data)
                        (format t "~&~a ~a" n
                                (average
                                 (mapcar (lambda (di)
                                           (* 1.0 (abs (- (bags num)
                                                          (- 1 (/ (apply #'+ di)
                                                                  (length di)))))))
                                         data))))

                      (f:range 1 100)
                      (apply #'mapcar #'list
                             (loop for i from 0 to 1000 collect (collect)))))
            :with '(:linespoint :title "percentage error")))))

(generate-2c-plot "images/p-2-c1.png" #'bag1 1 "Uncertainty for bag 1")
(generate-2c-plot "images/p-2-c2.png" #'bag2 2 "Uncertainty for bag 2")
(generate-2c-plot "images/p-2-c3.png" #'bag3 3 "Uncertainty for bag 3")
(generate-2c-plot "images/p-2-c4.png" #'bag4 4 "Uncertainty for bag 4")
(generate-2c-plot "images/p-2-c5.png" #'bag5 5 "Uncertainty for bag 5")
