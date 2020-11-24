(defstruct player (score 0) (wins 0))

(defun determine-winner (players)
  "Increment the WINS for the player with the highest score."
  (incf (player-wins (car (sort players #'>
                                :key #'player-score)))))
(defun length1 (list)
  (let ((len 0))
    (dolist (element list)
      (incf len))
    len))

(defun length1.1 (list)
  (let ((len 0))
    (dolist (element list)
      (incf len))))

(defun length2 (list)
  (let ((len 0))
    (mapc (lambda (element) (incf len))
          list)
    len))

;; There is no body here since the form of do is
;; (do ((variable inital next)...)
;;     (exit-test result)
;;   body...)
;; Bit confusing since it's not obv we are looping and how we go through the list 
(defun length3 (list)
  (do ((len 0 (1+ len))
       (l list (cdr l)))
      ((null l) len)))

;; Time for Loop!!!

(defun length4 (list)
  (loop for element in list
     count t))

(defun length5 (list)
  (loop for element in list
     summing 1))
(defun length6 (list)
  (loop with len = 0                    ; start with LEN = 0
     until (null list)                  ; and (until end of list)
     for element = (pop list)           ; on each iteration
     do (incf len)                      ; increment LEN by 1
     finally (return len)))             ; and return LEN


(defun length7 (list)
  (count-if #'true list))

(defun true (x) t)

(defun length8 (list)
  (if (null list)
      0
      (1+ (position-if #'true list :from-end t))))

(remove 1 '(1 2 3 2 1 0 -1))
;; (2 3 2 0 -1)

(remove 1 '(1 2 3 2 1 0 -1) :key #'abs)
;; (2 3 2 0)

(remove 1 '(1 2 3 2 1 0 -1) :test #'<)
;; (1 1 0 -1)

(remove 1 '(1 2 3 2 1 0 -1) :start 4)
;; (1 2 3 2 0 -1)

(remove-if #'oddp '(1 2 3 2 1 0 -1))
;; (2 2 0)

(remove-if-not #'oddp '(1 2 3 2 1 0 -1))
;; (1 3 1 -1)

(find-if #'evenp '(1 2 3 2 1 0 -1))
;; 2

(defun product (numbers)
  "Multiply all the numbers together to compute their product"
  (let ((prod 1))
    (dolist (n numbers prod)
      (if (= n 0)
          (return 0)
          (setf prod (* n prod))))))

;; Assoc Lists
(defparameter state-table
      '((AL . Alabama) (AK . Alaska) (AZ . Arizona) (AR . Arkansas)))

(assoc 'AK state-table)                 ; Searches for the key 
(rassoc 'Alaska state-table)            ; Searches for the value 

;; Hash Tables...  Better ASSOC lists

(defparameter table (make-hash-table))
(setf (gethash 'AL table) 'Alabama)
(setf (gethash 'AK table) 'Alaska)
(setf (gethash 'AZ table) 'Arizona)
(setf (gethash 'AE table) 'Arkansas)

(gethash 'AK table)

;; Property lists && association lists

;; a-list: ((key1 . val1) (key2 , val2) ...  (keyn . valn))
;; p-list: (key1 val1 key2 val2 ... keyn valn)



(setf (get 'AL 'stuff) 'Alabama)
(get 'AL 'state)


;; 3.9 Functions on Sets
(defparameter r '(a b c d))
(defparameter s '(c d e))
(intersection r s)                      ; (D C)
(union r s)                             ; (B A C D E)
(set-difference r s)                    ; (B A)
(subsetp s r)                           ; NIL <-- checks if all memebrs are in another
(adjoin 'd s)                           ; Adds an element to a set... unless if it's a duplicate

(bit-and    #*11110      #*11001)       ; #*1100
(logand     #b11110      #b11001)       ; 24





;; 3.5

(defstruct node
  name
  (yes nil)
  (no nil))

(defvar *db*
  (make-node :name 'animal
             :yes (make-node :name 'mamal)
             :no  (make-node :name 'vegetable
                             :no (make-node :name 'mineral))))

(defun questions (&optional (node *db*))
  (format t "~&Is it a ~a?" (node-name node))
  (case (read)
    ((y yes) (if (null (node-yes node))
                 (setf (node-yes node) (give-up))
                 (questions (node-yes node))))
    ((n no)  (if (null (node-no node))
                 (setf (node-no node) (give-up))
                 (questions (node-no node))))
    (it 'aha!)
    (t (format t "Replay with YES, NO, or IT if I have guessed it.")
       (questions node))))

(defun give-up ()
  (format t "~&I give up - whawt is it? ")
  (make-node :name (read)))



;; 3.11 Files-------------------------------------------------------------

(with-open-file (stream "test.text" :direction :output)
  (print '(hello there) stream)
  (princ 'goodbye stream))

(with-open-file (stream "test.text" :direction :input)
  (list (read stream) (read-char stream) (read stream)
        (read stream nil 'eof)))

(format t "~&~a plus ~s is ~d" "two" "two" 4) ; two plus "two" is 4


;; ~& (= new-line)

;; (= ~a princ)
(princ "two")

;; (= ~s prin1)
(prin1 "two")

(let ((numbers '(1 2 3 4 5)))
  (format t "~&~{~r~^ plus ~} is ~@r"
          numbers (apply #'+ numbers)))

;; ~^ exits from the loop if there are no more arguments

;; I plus II plus III plus IV plus V is XV

;; one plus two plus three plus four plus five is XV
;; 3.13 Debugging-------------------------------------------------------------
(proclaim '(optimize (debug 3)))

(describe 'make-string)
(make-string)

(ql:quickload "com.informatimago.common-lisp.lisp.stepper")

(stepper:step (+ 3 4 (* 5 6 (/ 7 8))))


;; 3.14 Anti-Bugging---------------------------------------------------------
(defun average (numbers)
  (if (null numbers)
      (progn
        (cerror "Use 0 as the average."
                "Average of the empty list is undefined.")
        0)
      (/ (reduce #'+ numbers)
         (length numbers))))

(defun sqr% (x)
  "Multiply x by itself."
  (check-type x number)
  (* x x))

(defun sqr (x)
  "Multiply x by itself."
  (assert (numberp x) (x))              ; The second argument gives it something to change
  (* x x))

;; 3.16 closures---------------------------------------------------------------
(defun adder (c)
  (lambda (x) (+ x c)))

(mapcar (adder 3) '(1 3 10))

(defun bank-account (balance)
  "Open a bank account starting with the given balance."
  (lambda (action amount)
    (case action
      (deposit  (setf balance (+ balance amount)))
      (withdraw (setf balance (- balance amount))))))

(defparameter my-account (bank-account 500))
(funcall my-account 'withdraw 75)


(defvar a 'global-a)
(defvar *b* 'global-b)
(defun fn () *b*)
(let ((a 'local-a)
      (*b* 'local-b))
  (list a *b* (fn) (symbol-value 'a) (symbol-value '*b*)))

;; 3.19 More about Parameters-----------------------------------------------
(defun math-quiz (&key (op '+) (range 100) (n 10))
  "Ask the user a series of math problems."
  (dotimes (i n)
    (problem (random range) op (random range))))

(defun problem (x op y)
  "Ask a math problem, read a reply, and say if it is correct."
  (format t "~&How much is ~d ~a ~d?" x op y)
  (finish-output)
  (if (eql (read) (funcall op x y))
      (princ "Correct!")
      (princ "Sorry, that's not right.")))

