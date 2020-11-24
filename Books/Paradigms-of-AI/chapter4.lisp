(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
   according to the keywords.
   Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(find-all 1 '(1 2 3 4 5 6 7 1) :test #'= :key #'abs)

;; GPS: The General Problem Solver 



;; 4.1 Stage 1: Description---------------------------------

;; 4.2 Stage 2: Specification-------------------------------

;; (GPS '(unkown poor) '(rich famous) list-of-ops)

;; 4.3 Stage 3: Implementation------------------------------

;; ---------------Top level function--------------
;; GPS                Solve a goal from a sate using a list of operators
;; ---------------Special Variables---------------
;; *state*            The current state: list of conditions
;; *ops*              A list of available operators
;; ---------------Data Types----------------------
;; op                 An operation with preconds, add-list and del-list
;; ---------------Functions-----------------------
;; achieve            Achieve an individual goal
;; appropriate-p      Decide if an operator is appropriate for a goal
;; apply-op           Apply operator to current state 
;; ---------------Selected COmmon Lisp Function---
;; member
;; set-difference
;; union 


(defvar *state* nil "The current state: a list of conditions")

(defvar *ops* nil "A list of available operators")

(defstruct op
  "An operation"
  (action nil) (preconds nil) (add-list nil) (del-list nil))

(defun GPS% (*state* goals *ops*)
  "General Problem Solver: Achieve all goals using *ops*."
  (when (every #'achieve goals) 'solved))

(defun achieve (goal)
  "A goal is achieved if it already holds, or if
   there is an appropriate op for it that is applicable."
  (or (member goal *state*)
      (some #'apply-op
            (find-all goal *ops* :test #'appropriate-p))))

(defun appropriate-p (goal op)
  "An op is appropriate to a goal if it is in its add list."
  (member goal (op-add-list op)))

(defun apply-op (op)
  "print a message and update *state* if op is applicable."
  (when (every #'achieve (op-preconds op))
    (print (list  'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))
    t))



;; 4.4 Stage 4: Test----------------------------------------
(make-op :action 'drive-son-to-school
         :preconds '(son-at-home car-works)
         :add-list '(son-at-school)
         :del-list '(son-at-home))
(defparameter *school-ops*
  (list
   (make-op :action 'drive-son-to-school
            :preconds '(son-at-home car-works)
            :add-list '(son-at-school)
            :del-list '(son-at-home))
   
   (make-op :action 'shop-installs-battery
            :preconds '(car-needs-battery shop-knows-problem shop-has-money)
            :add-list '(car-works))
   
   (make-op :action 'tell-shop-problem
            :preconds '(in-communication-with-shop)
            :add-list '(shop-knows-problem))
   
   (make-op :action 'telephone-shop
            :preconds '(know-phone-number)
            :add-list '(in-communication-with-shop))

   (make-op :action 'look-up-number
            :preconds '(have-phone-book)
            :add-list '(know-phone-number))

   (make-op :action 'give-shop-money
            :preconds '(have-money)
            :add-list '(shop-has-money)
            :del-list '(have-money))))

(gps% '(son-at-home car-needs-battery have-money have-phone-book)
      '(son-at-school)
      *school-ops*)

(gps% '(son-at-home car-needs-battery have-money)
      '(son-at-school)
      *school-ops*)

(gps% '(son-at-home car-works)
      '(son-at-school)
      *school-ops*)

;; 4.5 Improving the program--------------------------------
(defun achieve-all (goals)
  "Try to achieve each goal, then make sure they still hold."
  (and (every #'achieve goals) (subsetp goals *state*)))

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified" 
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

(defun debug% (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(defun undebug (&rest ids)
  "Stop dbg on the ids. With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids)
                      nil
                      (set-difference *dbg-ids* ids))))

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified"
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ " " *debug-io*))
    (apply #'format *debug-io* format-string args)))



;; 4.11 GPS version 2: A More General Problem Solver-----------------------------------------

(defun executing-p (x)
  "Is x of the form: (executing ...) ?"
  (starts-with x 'executing))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (car list) x)))

(defun convert-op (op)
  "Make op conform to the (Executing op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'executing (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (Executing op) convention."
  (convert-op
   (make-op :action action :preconds preconds
            :add-list add-list :del-list del-list)))

(mapc #'convert-op *school-ops*)


(defun GPS (state goals &optional (*ops* *ops*))
  "General Problem Solver: from state, achieve goals using *ops*."
  (remove-if #'atom (achieve-all (cons '(start) state) goals nil)))

