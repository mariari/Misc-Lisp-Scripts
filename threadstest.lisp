(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "bordeaux-threads")
  (ql:quickload "bt-semaphore")
  (ql:quickload "inferior-shell")
  (ql:quickload "alexandria")
  (ql:quickload '(:fare-quasiquote-readtable
                  :fare-quasiquote
                  :let-over-lambda
                  :trivia)))


(defpackage #:thread-test
  (:use #:let-over-lambda)
  (:import-from #:alexandria #:parse-body)
  (:shadowing-import-from #:let-over-lambda #:when-match #:if-match #:symb)
  (:use #:inferior-shell)
  (:shadowing-import-from #:inferior-shell #:<>)
  (:use #:functions
        #:bordeaux-threads
        #:macros
        #:trivia
        #:common-lisp)
  (:import-from #:bt-semaphore #:semaphore-count)
  (:export :pmapcar
           :pmap
           :plmapcar
           :defun-s!
           :my-command-line :split-by-delim))

(in-package :thread-test)

(named-readtables:in-readtable :fare-quasiquote)

(defun num-threads ()
  (uiop:os-cond ((uiop:os-unix-p)    (read-from-string (run/s `(nproc --all)))) ; works
                ((uiop:os-windows-p) (read-from-string (run/s `(echo %NUMBER_OF_PROCESSORS%)))) ; not tested
                ((uiop:os-macosx-p)  (read-from-string (run/s `(sysctl -n hw.ncpu)))) ; not tested
                (t                   8)))         ; if it's not a supported OS, just put the cores to 8


(defun ignored-threads-imp (&optional extra-threads-list)
  (let ((ignore (append extra-threads-list (list  "swank-indentation-cache-thread"
                                                  "reader-thread" "control-thread"
                                                  "Swank Sentinel" "main thread")))
        (count 0))
    (mapc (lambda (x)
            (when (member (thread-name x) ignore :test #'equal)
              (incf count)))
          (all-threads))
    count))

(defun ignored-threads (&optional extra-threads-list)
  (let ((ignore (append extra-threads-list (list  "swank-indentation-cache-thread"
                                                  "reader-thread"  "control-thread"
                                                  "Swank Sentinel" "main thread"))))
    (reduce (lambda (x y)
              (if (member (thread-name y) ignore :test #'equal)
                  (1+ x) x))
            (all-threads) :initial-value 0)))

(defun ignored-threads-func2 (&optional extra-threads-list)
  (let ((ignore (append extra-threads-list (list  "swank-indentation-cache-thread"
                                                  "reader-thread"  "control-thread"
                                                  "Swank Sentinel" "main thread"))))
    (length (remove-if-not (lambda (x) (member (thread-name x) ignore :test #'equal))
                           (all-threads)))))

(defun ignored-threads-func1 (&optional extra-threads-list)
  (let ((ignore (append extra-threads-list (list  "swank-indentation-cache-thread"
                                                  "reader-thread"  "control-thread"
                                                  "Swank Sentinel" "main thread"))))
    (length (intersection ignore (mapcar 'thread-name (all-threads)) :test 'equal))))


(defun num-used-threads ()
  (length (all-threads)))

(defparameter num-threads-offset (+ (num-threads) (ignored-threads)))

(defun num-open-threads ()
  (- num-threads-offset (num-used-threads)))

(defun plmapcar (fn list &rest more-lists)
  "works like mapcar except every process is wrapped around a new thread and the computation gets passed
   onto the user to evaluate when they wish and evaluates (parallel lazy mapcar)"
  (apply (f:curry mapcar (lambda (&rest x)
                           (make-thread (lambda () (apply fn x))))
                  list)
         more-lists))

(macrolet ((pmap-gen (&optional result-type)
             ;; mapcar is faster than map so inject mapcar instead of map if a result type is passed here
             `(let* ((thread-lim (1- (num-open-threads)))
                     (mutex (make-semaphore :count thread-lim))
                     (finished (make-semaphore :count 0))
                     (vals (prog1 (apply (f:curry ,@(if result-type
                                                        `(map result-type)
                                                        `(mapcar))
                                                  (lambda (&rest x)
                                                    (wait-on-semaphore mutex)
                                                    (make-thread (lambda () (prog1 (apply fn x)
                                                                         (signal-semaphore mutex)))))
                                                  list)
                                         more-lists)
                             (signal-semaphore finished))))
                (wait-on-semaphore finished)
                ,(if result-type
                     `(map result-type #'join-thread vals)
                     `(mapcar #'join-thread vals)))))

  (defun pmap (result-type fn list &rest more-lists)
    (pmap-gen result-type))

  (defun pmapcar (fn list &rest more-lists)
    (pmap-gen)))


;; Generate a macro that returns a function and injects a semaphore in it's place

;; s! does semaphore wait before the p! variable/statement
;; p! marks that part of the code for parallization
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun s!-symbol-p (s)
    (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (string= (symbol-name s)
                "S!"
                :start1 0
                :end1 2)))

  ;; From LOL
  (defun my-symb (&rest args)
    (values (intern (apply #'mkstr args))))

  ;; from LOL

  (defun s!-symbol-to-function (s)
    (my-symb (subseq (mkstr s) 2))))

;; Poorest way to do it
(defmacro defun-s!% (name args &rest body)
  "creates a defun with the extra functionality of stating s! in front of a function to make any
   code within that functions scope happen between a (wait-on-semaphore) and a (signal-semaphore).
   Note that you can overload (num-open-threads) before the function is declared to control the initial
   semaphore value (flet ((num-open-threads () 1)) for 1"
  (let ((g!lock (gensym "lock"))
        (g!x    (gensym "x"))
        (syms (remove-duplicates
               (remove-if-not #'s!-symbol-p
                              (flatten body)))))
    (multiple-value-bind (body declarations docstring)
        (parse-body body :documentation t)
      `(defun ,name ,args
         ,@(when docstring
             (list docstring))
         ,@declarations
         (let ((,g!lock (make-semaphore :count (num-open-threads) :name "auto-sym"))) ; semaphore creation to (num-open-threads)
           (flet ,(mapcar (lambda (s)
                            `(,s (&rest ,g!x)
                                 (prog2
                                     (wait-on-semaphore ,g!lock)
                                     ;; (apply #',(s!-symbol-to-function s) ,g!x)    ; doesn't work for special forms or macros
                                     ;;  This actually isn't too slow!!!
                                     (eval (eval `(cons ',',(s!-symbol-to-function s) ',,g!x)))
                                     (signal-semaphore ,g!lock))))
                          syms)
             ,@body))))))


;; Parser with if statements
(defmacro defun-s!%% (name args &rest body)
  "creates a defun with the extra functionality of stating s! in front of a function to make any
   code within that functions scope happen between a (wait-on-semaphore) and a (signal-semaphore).
   Note that you can overload (num-open-threads) before the function is declared to control the initial
   semaphore value (flet ((num-open-threads () 1)) for 1"
  (let ((g!lock (gensym "lock"))
        (g!y    (gensym)))
    (multiple-value-bind (body declarations docstring)
        (parse-body body :documentation t)
      `(defun ,name ,args
         ,@(when docstring
             (list docstring))
         ,@declarations
         (let ((,g!lock (make-semaphore :count (num-open-threads) :name "auto-sym"))) ; semaphore creation to (num-open-threads)
           ,@(mapcar (alambda (x)
                       (if (and (not (null x)) (listp x))
                           ;; Checking to see if the function is in the form (fn ...)
                           (if (s!-symbol-p (car x))
                               `(prog2
                                    (wait-on-semaphore ,g!lock)
                                    ,(cons (s!-symbol-to-function (car x))
                                           (mapcar #'self (cdr x)))
                                    (signal-semaphore ,g!lock))
                               (mapcar #'self x))
                           ;;  we are passing the s! function/macro to a HOF (cdr of a () set)
                           (if (s!-symbol-p x)
                               `(lambda (&rest ,g!y)
                                  (prog2
                                      (wait-on-semaphore ,g!lock)
                                      (eval (eval `(cons ',',(s!-symbol-to-function x) ',,g!y)))
                                      (signal-semaphore ,g!lock)))
                               x)))
                     body))))))


;; Parser with proper pattern matching
(defmacro defun-s! (name args &rest body)
  "creates a defun with the extra functionality of stating s! in front of a function to make any
   code within that functions scope happen between a (wait-on-semaphore) and a (signal-semaphore).
   Note that you can overload (num-open-threads) before the function is declared to control the initial
   semaphore value (flet ((num-open-threads () 1)) for 1"
  (let ((g!lock (gensym "lock"))
        (g!y    (gensym)))
    (multiple-value-bind (body declarations docstring)
        (parse-body body :documentation t)
      `(defun ,name ,args
         ,@(when docstring
             (list docstring))
         ,@declarations
         (let ((,g!lock (make-semaphore :count (num-open-threads) :name "auto-sym"))) ; semaphore creation to (num-open-threads)
           ,@(mapcar (alambda (x)
                       (match x
                         ((guard (list* a b) (s!-symbol-p a))
                                 `(prog2 (wait-on-semaphore ,g!lock)
                                      ,(cons (s!-symbol-to-function a)
                                             (mapcar #'self b))
                                    (signal-semaphore ,g!lock)))
                         ((guard a (s!-symbol-p a)) ; the s! symbol must be on the cdr of the list since we are not seeing it
                                 `(lambda (&rest ,g!y)   ; at the start of a list but instead as an atom
                                    (prog2
                                        (wait-on-semaphore ,g!lock)
                                        (eval (eval `(cons ',',(s!-symbol-to-function x) ',,g!y)))
                                        (signal-semaphore ,g!lock))))
                         ((list* _ _) (mapcar #'self x)) ; do this pattern matching again on the list
                         (a a)))
                     body))))))

(defun-s! test (arg1 arg2)
  (s!+ arg1 arg2))


;; A side by side comparison of writer-s! vs the same function with no g!
;; (flet ((num-open-threads () 1))
;;   (defun-s! writer-s! ()
;;     '(flet ((reader ()
;;              (mapcar (lambda (x)
;;                        (s!progn (sleep .3)
;;                                 (print x)) x)
;;                      (range 10))))

;;       (let ((curr (make-thread #'reader)))
;;         (dotimes (i 11)
;;           (s!progn
;;            (sleep .1)
;;            (format t "~%waiting ~d" i)
;;            (force-output)
;;            (sleep .1)))
;;         (join-thread curr)))))

(let ((lock (make-semaphore :count 1 :name "test")))
  (defun writer ()
    (let ((curr (make-thread #'reader)))
      (dotimes (i 11)
        (wait-on-semaphore lock)
        (sleep .1)
        (format t "~%waiting ~d" i)
        (force-output)
        (sleep .1)
        (signal-semaphore lock))
      (join-thread curr)))

  (defun reader ()
    (mapcar (lambda (x)
              (thread-yield)
              (wait-on-semaphore lock)
              (sleep .3)
              (print x)
              (signal-semaphore lock) x)
            (list:range 10))))
;; What!??!?!
;; works

;;; The testing part of the file!-------------------------------------------------------------------

;; (run/ss `(pipe (ls) (grep "he")))
;; (run/ss `(grep ))

;; (run/ss `(pipe (echo ,(lss "~/")) (grep "he")))

;; (run/ss `(pipe (echo (+ hel "lo,") world) (tr "hw" "HW") (sed -e "s/$/!/")))

;; (time (split-by-delim #\linefeed (run/ss `(pipe (echo ,(lss "~/")) (grep ".") (sed -e "s/o/0/g")))))
;; (time (split-by-delim #\linefeed (run/ss `(pipe (ls /home/loli/) (grep ".") (sed -e "s/o/0/g")))))
;; (time (run/lines `(pipe (ls /home/loli/) (grep ".") (sed -e "s/o/0/g"))))

;; (let ((stream (make-string-output-stream)))
;;   (run-program "ls ~" :output stream)
;;   (get-output-stream-string stream))

;; (detect-os)
;; (uiop:read-little-endian)

;; (time (mapcar (lambda (x) (make-thread (lambda () (mapcar (lambda (y) (+ x 1 2 3 4 y)) (list 1 2 3 4 5))))) (range 1000)))


;; (time (make-thread (lambda ())))
;; (pmapcar (lambda (x) (declare (ignore x)) (sleep 1)) (range 20))
;; (time (pmapcar (lambda (x y) (sleep .3)  (+ 1 2 3 x y)) (range 20) (range 13)))


;; For History--------------------------------------------------------------------------------------
;; This is just a spinlock version of pmap and pmapcar
(macrolet ((pmap-gen (&optional result-type)
             ;; mapcar is faster than map so inject mapcar instead of map if a result type is passed here
             `(let* ((thread-lim (1- (num-open-threads)))
                     (mutex (make-semaphore :count thread-lim))
                     (vals (apply
                            (f:curry ,@(if result-type
                                           `(map result-type)
                                           `(mapcar))
                                     (lambda (&rest x)
                                       (wait-on-semaphore mutex)
                                       (make-thread (lambda () (prog1 (apply fn x)
                                                            (signal-semaphore mutex)))))
                                     list)
                            more-lists)))
                (loop :while (/= (semaphore-count mutex) thread-lim))
                ,(if result-type
                     `(map result-type #'join-thread vals)
                     `(mapcar #'join-thread vals)))))

  (defun pmap* (result-type fn list &rest more-lists)
    (pmap-gen result-type))

  (defun pmapcar* (fn list &rest more-lists)
    (pmap-gen)))




(defun fibonacci (n)
  (labels ((rec (n)
             (if (= n 0)
                 (list 0 1)
                 (let* ((ls (rec (truncate n 2)))
                        (z (car ls))
                        (f (cadr ls))
                        (c (* z (- (* 2 f) z)))
                        (d (+ (* f f) (* z f))))
                   (if (= (mod n 2) 0)
                       (list c d)
                       (list d (+ c d)))))))
    (car (rec n))))
