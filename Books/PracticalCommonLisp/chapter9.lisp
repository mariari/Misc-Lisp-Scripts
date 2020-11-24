;;; Testing
(defvar *test-name* nil)

;; nice way, but missing important output 
(defun test-+% ()
  (and (= (+ 1 2) 3)
       (= (+ 1 2 3) 6)
       (= (+ -1 -3) -4)))

;; Pain in the ass way 
(defun test-+%% ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))


;; REFACTORING
;; (defun report-result (result form)
;;   "Takes a result and the form to evaluate to check if it passes the test"
;;   (format t "~:[FAIL~;pass~]... ~a: ~a~%" result *test-name* form) result)

(defun test-+%%% ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

;; Refactor that code even more with a macro.. let's make it less error prone
(defmacro check% (form)
  `(report-result ,form ',form))

(defun test-+%%%% ()
  (check% (= (+ 1 2) 3))
  (check% (= (+ 1 2 3) 6))
  (check% (= (+ -1 -3) -4)))

;; (mapcar (lambda (x) (check% x)) '((= (+ -1 -3) -4) (= (+ 1 2 3) 6)))

;; Still a bitch to call check this many times 

(defmacro check%% (&body forms)
  `(progn ,@(mapcar (lambda (x) `(report-result ,x ',x)) forms)))

(defun test-+% ()
  (check (= (+ 1 2) 3)
         (= (+ 1 2 3) 6)
         (= (+ 1 2 3) 5)
         (= (+ -1 -3) -4)))

;;; Fixing the issue with the return-----------------------------------------
;; We could use and, but that short circuits and won't tell us the result if one of them fails 

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(mapcar (lambda (x) `(,x (gensym))) names)
     ,@body))

;; WTF author, why make this a macro
(defmacro combine-results% (&body forms)
  "Works like the and operator, but does not short circuit"
  (let ((result (gensym)))
    `(let ((,result t))                 ; If one of the tests turns out to be nil, then result becomes nil 
       ,(mapcar (lambda (x) `(unless ,x (setf ,result nil))) forms) ,result)))

(defun combine-results (&rest forms)
  "Works like the and operator, but does not short circuit"
  (let ((result t))                     ; If one of the tests turns out to be nil, then result becomes nil 
    (mapcar (lambda (x) (unless x (setf result nil))) forms) result))


;; (defun combine-results (&rest forms)
;;   "Works like the and operator, but does not short circuit"
;;   (let ((result t))                     ; If one of the tests turns out to be nil, then result becomes nil 
;;     (mapcar (lambda (x) (unless x (setf result nil))) forms) result))

;; (defmacro check (&body forms)
;;   `(combine-results ,@(mapcar (lambda (x) `(report-result ,x ',x)) forms)))


(defun test-*% ()
  (check (= (* 2 2) 4)
         (= (* 3 5) 15)))

(defun test-arithmetic ()
  (combine-results (test-+) (test-*)))

;; The problem now is that we don't know where the tests are coming from, so we changed the format expression, but in order to take advantage of it
;; We have to define stuff like this... which is an incomplete abstraction
(defun test-+%% ()
  (let ((*test-name* 'test-+))
    (check (= (+ 1 2) 3)
           (= (+ 1 2 3) 6)
           (= (+ 1 2 3) 5)
           (= (+ -1 -3) -4))))

;; Organization of tests-------------------------------------------------------------------------------------------------------------------------------

;; Useful for the first level or organization , but with thousands upon thousands of tests this will only be annoying 
(defmacro deftest% (name parameters &body body)
  "allows the user to defun a test"
  `(defun ,name ,parameters
     (let ((*test-name* ',name))
       ,@body)))

(deftest test-+ ()
  (check (= (+ 1 2) 3)
         (= (+ 1 2 3) 6)
         (= (+ -1 -3) -4)))

(deftest test-* ()
  (check (= (* 2 2) 4)
         (= (* 3 5) 15)))
;; Hierarchy of tests----------------------------------------------------------------------------------------------------------------------------------

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call other test functions
   or use 'check' to run individual test cases"
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(defmacro check (&body forms)
  `(combine-results ,@(mapcar (lambda (x) `(report-result ,x ',x)) forms)))

(defun combine-results (&rest forms)
  "Works like the and operator, but does not short circuit"
  (let ((result t))                     ; If one of the tests turns out to be nil, then result becomes nil 
    (mapcar (lambda (x) (unless x (setf result nil))) forms) result))

(defun report-result (result form)
  "Takes a result and the form to evaluate to check if it passes the test"
  (format t "~:[FAIL~;pass~]... ~a: ~a~%" result *test-name* form) result)

(deftest test-arithmetic ()
  (combine-results
    (test-+)
    (test-*)))
