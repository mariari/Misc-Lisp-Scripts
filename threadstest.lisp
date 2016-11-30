(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "bordeaux-threads")
  (ql:quickload "bt-semaphore")
  (ql:quickload "trivia")
  (ql:quickload "inferior-shell")
  (ql:quickload "alexandria")
  (asdf:load-system :uiop)
  ;; (load "~/Documents/Workspace/Lisp/CommonLisp/functions.lisp")
  (load "./CL/functions.lisp"))


(defpackage #:shell
  (:nicknames #:fun :times)
  (:use #:let-over-lambda)
  (:import-from #:alexandria #:parse-body)
  (:shadowing-import-from #:let-over-lambda #:when-match #:if-match #:symb)
  (:use #:trivia)
  (:shadowing-import-from #:trivia #:<>)
  (:use #:inferior-shell
        #:uiop
        #:functions
        #:bordeaux-threads
        #:bt-semaphore
        #:common-lisp))

(in-package :shell)

(run/ss `(pipe (echo (+ hel "lo,") world) (tr "hw" "HW") (sed -e "s/$/!/")))


(defmacro curry (fn . args)
  "Creates a partially applied function that takes 1 argument if it is a macro
   (a limitation of &rest closures in CL) and multiple if it is a function"
  (if (functionp (macro-function fn))
      `(currym ,fn ,@args)
      `(curryf #',fn ,@args)))

(defmacro currym (fn . args)
  "Creates a partially applied function that takes 1 argument"
  (let ((arg (gensym)))
    `(lambda (,arg) (,fn ,@args ,arg))))

(declaim (ftype (function (function &rest t) function) curryf)
         (inline curryf))
(defun curryf (fn &rest args)
  "Creates a partially applied function that takes many argument"
  (lambda (&rest args2) (apply fn (append args args2))))

;; (run/ss `(pipe (ls) (grep "he")))
;; (run/ss `(grep ))

;; (run/ss `(pipe (echo ,(lss "~/")) (grep "he")))

(time (split-by-delim #\linefeed (run/ss `(pipe (echo ,(lss "~/")) (grep ".") (sed -e "s/o/0/g")))))
(time (split-by-delim #\linefeed (run/ss `(pipe (ls /home/loli/) (grep ".") (sed -e "s/o/0/g")))))
(time (run/lines `(pipe (ls /home/loli/) (grep ".") (sed -e "s/o/0/g"))))

(let ((stream (make-string-output-stream)))
  (run-program "ls ~" :output stream)
  (get-output-stream-string stream))

(detect-os)
;; (uiop:read-little-endian)

(time (mapcar (lambda (x) (make-thread (lambda () (mapcar (lambda (y) (+ x 1 2 3 4 y)) (list 1 2 3 4 5))))) (range 1000)))


(time (make-thread (lambda ())))


(defun num-threads ()
  (os-cond ((os-unix-p)    (read-from-string (run/s `(nproc --all)))) ; works
           ((os-windows-p) (read-from-string (run/s `(echo %NUMBER_OF_PROCESSORS%)))) ; not tested
           ((os-macosx-p)  (read-from-string (run/s `(sysctl -n hw.ncpu)))) ; not tested
           (t              8)))         ; if it's not a supported OS, just put the cores to 8


(defun ignored-threads (&optional extra-threads-list)
  (let ((ignore (append extra-threads-list (list  "swank-indentation-cache-thread"
                                                  "reader-thread" "control-thread"
                                                  "Swank Sentinel" "main thread")))
        (count 0))
    (mapc (lambda (x) (when (member (thread-name x) ignore :test #'equal)
                   (incf count)))
          (all-threads))
    count))


(defun num-used-threads ()
  (length (all-threads)))

(defparameter *num-threads-offset* (+ (num-threads) (ignored-threads)))

(defun num-open-threads ()
  (- *num-threads-offset* (num-used-threads)))

(defun plmapcar (fn list &rest more-lists)
  "works like mapcar except every process is wrapped around a new thread and the computation gets passed
   onto the user to evaluate when they wish and evaluates (parallel lazy mapcar)"
  (apply (curry mapcar (lambda (&rest x)
                         (make-thread (lambda () (apply fn x))))
                       list)
         more-lists))

(macrolet ((pmap-gen (&optional result-type)
             ;; mapcar is faster than map so inject mapcar instead of map if a result type is passed here
             `(let* ((thread-lim (1- (num-open-threads)))
                     (mutex (make-semaphore :count thread-lim))
                     (vals (apply
                            (curry ,@(if result-type
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
  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  ;; from LOL

  (defun s!-symbol-to-function (s)
    (symb (subseq (mkstr s) 2))))


(defmacro defun-s! (name args &rest body)
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

(defun-s! test (arg1 arg2)
  (s!+ arg1 arg2))

;; A side by side comparison of writer-s! vs the same function with no g!
(flet ((num-open-threads () 1))
  (defun-s! writer-s! ()
    (flet ((reader ()
             (mapcar (lambda (x)
                       (s!progn (sleep .3)
                                (print x)) x)
                     (range 10))))

      (let ((curr (make-thread #'reader)))
        (dotimes (i 11)
          (s!progn
           (sleep .1)
           (format t "~%waiting ~d" i)
           (force-output)
           (sleep .1)))
        (join-thread curr)))))

(let ((lock (make-semaphore  :count 1 :name "test")))
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
            (range 10))))
;; What!??!?!
;; works

;; (pmapcar (lambda (x) (declare (ignore x)) (sleep 1)) (range 20))
;; (time (pmapcar (lambda (x y) (sleep .3)  (+ 1 2 3 x y)) (range 20) (range 13)))
