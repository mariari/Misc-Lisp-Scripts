(ql:quickload "bordeaux-threads")
(ql:quickload "bt-semaphore")
(ql:quickload "trivia")
(ql:quickload "inferior-shell")
(asdf:load-system :uiop)
(load "~/Documents/Workspace/Lisp/CommonLisp/functions.lisp")

(defpackage #:shell
  (:nicknames #:fun :times)
  (:use #:trivia)
  (:use #:inferior-shell)
  (:use #:uiop)
  (:use #:functions)
  (:use #:bordeaux-threads)
  (:use #:bt-semaphore)
  (:use #:common-lisp))

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

(time (mapcar (lambda (x) (make-thread (lambda () (mapcar (lambda (x) (+ x 1 2 3 4)) (list 1 2 3 4 5))))) (range 1000)))


(time (make-thread (lambda ())))

(let ((lock (make-semaphore  :count 1 :name "test")))
  (defun writer ()
    (let ((curr (make-thread #'reader)))
      (dotimes (i 10)
        (wait-on-semaphore lock)
        (sleep 1)
        (format t "~%waiting ~d" i)
        (sleep 1)
        (signal-semaphore lock))
      (join-thread curr)))
  
  (defun reader ()
    (mapcar (lambda (x)
              (thread-yield)
              (wait-on-semaphore lock)
              (print x)
              (signal-semaphore lock) x)
            (range 10))))


(defun num-threads ()
  (os-cond ((os-unix-p)    (read-from-string (run/s `(nproc --all)))) ; works
           ((os-windows-p) (read-from-string (run/s `(echo %NUMBER_OF_PROCESSORS%)))) ; not tested
           ((os-macosx-p)  (read-from-string (run/s `(sysctl -n hw.ncpu)))) ; not tested
           (t              8)))         ; if it's not a supported OS, just put the cores to 8


(defun ignored-threads (&optional extra-threads-list)
  (let ((ignore (append extra-threads-list (list  "swank-indentation-cache-thread" "reader-thread" "control-thread" "Swank Sentinel" "main thread")))
        (count 0)
        (curr (all-threads)))
    (mapc (lambda (x) (when (member (thread-name x) ignore :test #'equal)
                   (incf count)))
          curr)
    count))


(defun num-used-threads ()
  (length (all-threads)))

(defun num-open-threads ()
  (- *num-threads-offset* (num-used-threads)))

(defparameter *num-threads-offset* (+ (num-threads) (ignored-threads)))

(defun pmapcar (fn list &rest more-lists)
  "works like mapcar except every process is wrapped around a new thread and the computation gets passed
   onto the user to evaluate when they wish"
  (apply (curry mapcar (lambda (&rest x)
                         (make-thread (lambda () (apply fn x))))
                       list)
         more-lists))

(defun pmap-now (fn list &rest more-lists)
  (let* ((thread-lim (1- (num-open-threads)))
         (mutex (make-semaphore :count thread-lim))
         (vals (apply
                (curry mapcar (lambda (&rest x)
                                (wait-on-semaphore mutex)
                                (make-thread (lambda () (let ((val (apply fn x)))
                                              (signal-semaphore mutex)
                                              val))))
                       list)
                more-lists)))
    (loop :while (/= (semaphore-count mutex) thread-lim))
    (mapcar #'join-thread vals)))

;; What!??!?!
;; works

;; (pmap-now (lambda (x) (declare (ignore x)) (sleep 1)) (range 20))
