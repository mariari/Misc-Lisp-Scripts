;; if there are items in the list then +1, else remove the first element and call again 
(defun my-length (list)
  (if list
      (1+ (my-length (cdr list)))
      0))

(my-length '(list with four symbols))
;; does the same as my-length 
(length '(list with four symbols))
;; / 1 0 should crash, but it is never evaluated 
(if (oddp 5)
    'odd-number
    (/ 1 0))


(defvar *arch-enemy* nil)

(defun pudding-eater (person)
  (case person
    ((henry)    (setf *arch-enemy* 'stupid-lisp-alien)
               '(curse you lisp alien – you ate my pudding))
    ((johnny)   (setf *arch-enemy* 'useless-old-johnny) 
               '(i hope you choked on my pudding johnny))
    (otherwise '(why you eat my pudding stranger ?))))

(pudding-eater 'johnny)

;; by annon from Lainchan 
(defun euler-2 (count x y)
  (when (evenp y) (setf count (+ count y)))
  (if (> y 4000000) (print count)
      (euler-2 count y (+ x y))))
(euler-2 0 1 40)




;; by annon on Lainchan
;; test
(defun throw-die ()
  (1+ (random 6)))

(defun throw-dice ()
  (list (throw-die) (throw-die)))

(defun snake-eyes-p (throw)
  
  (if (and (= 1 (first throw))
           (= 1 (second throw)))
      t))

(defun boxcarsp (throw)
  (if (and (= 6 (first throw))
           (= 6 (second throw)))
      t))

(defun instant-win-p (throw)
  (if (or (= 7 (+ (first throw) 
                  (second throw)))
          (= 11 (+ (first throw)
                   (second throw))))
      t))

(defun instant-loss-p (throw)
  (if (or (boxcarsp throw)
          (snake-eyes-p throw)
          (= 3 (+ (first throw)
                  (second throw))))
      t))

(defun say-throw (throw)
  (cond ((boxcarsp throw) 'boxcars!)
        ((snake-eyes-p throw) 'snake-eyes!)
        (t (+ (first throw)
              (second throw)))))

(defun craps ()
  (let* ((throw (throw-dice))
         (result (say-throw throw)))
    (cond ((instant-win-p throw) 
           (format t "Throw ~a and ~a - ~a - you win!" 
                   (first throw)
                   (second throw)
                   result))
          ((instant-loss-p throw) 
           (format t "Throw ~a and ~a - ~a - you lose!" 
                   (first throw)
                   (second throw)
                   result))
          (t (format t "Throw ~a and ~a - your point is ~a" 
                     (first throw)
                     (second throw)
                     result)))))

(defun try-for-point (point)
  (let* ((throw (throw-dice))
         (result (say-throw throw)))
    (cond ((eq point result) 
           (format t "Throw ~a and ~a - ~a - you win!" 
                   (first throw)
                   (second throw)
                   result))
          ((eq 7 result) 
           (format t "Throw ~a and ~a - ~a - you lose!" 
                   (first throw)
                   (second throw)
                   result))
          (t (format t "Throw ~a and ~a - ~a - roll again!" 
                     (first throw)
                     (second throw)
                     result)))))

;; wtf lisp-1
(let ((+ 1))
  (+ + 2))
(find-if )
