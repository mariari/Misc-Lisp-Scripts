;; Here we go with the proper system

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *mark-safe-p* nil)
(defvar *front-door-locked-p* nil)
(defvar *back-door-locked-p* nil)

(defvar *toplevel-restarts* '())

(defvar *excuses*
  '("Kate did not divide her program into sections properly!"
    "I was borrowing Kate's books on mainframe programming!"
    "I had COBOL-related homework and hoped Kate could help me!"))

(restart-bind ((escape-through-front-door
                 #'perform-escape-through-front-door
                 :test-function
                 #'escape-through-front-door-p))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun parents-come-back ()
  (format t ";; Uh oh- Kate's parents are back!~%")
  (try-to-hide-mark)
  (if *mark-safe-p*
      (format t ";; Whew... We're safe! For now.~%")
      (error "We do not want to be here")))

(defun try-to-hide-mark ()
  (let ((restarts (compute-relevant-restarts)))
    (if restarts
        (let ((restart (first restarts)))
          (format t ";; Performing ~A.~%" (restart-name restart))
          (invoke-restart restart))
        (format t ";; Kate cannot hide Mark!~%"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Escape hatches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-relevant-restarts (&optional condition)
  (set-difference (compute-restarts condition) *toplevel-restarts*))

(defun call-with-home-restarts (thunk)
  (let ((*toplevel-restarts* (compute-restarts)))
    (restart-bind ((escape
                   #'perform-escape-through-front-door
                   :test-function
                   #'escape-through-front-door-p)
                 (escape
                   #'perform-escape-through-back-door
                   :test-function
                   #'escape-through-back-door-p))
      (funcall thunk))))

(restart-bind ((escape-through-front-door
                 #'perform-escape-through-front-door
                 :test-function
                 #'escape-through-front-door-p))
  (let ((*mark-safe-p* nil)
        (*front-door-locked-p* nil))
    (let* ((restarts (compute-restarts))
           (names (mapcar #'restart-name restarts)))
      (format t "~{;; ~A~%~}" names))))

(call-with-home-restarts
 (lambda ()
   (let ((*mark-safe-p* nil)
         (*front-door-locked-p* nil)
         (*back-door-locked-p* nil))
     (let* ((restarts (compute-relevant-restarts))
            (names (mapcar #'restart-name restarts)))
       (format t "~{;; ~A~%~}" names)))))

(call-with-home-restarts
 (lambda ()
   (let ((*mark-safe-p* nil)
         (*front-door-locked-p* nil)
         (*back-door-locked-p* nil))
     (parents-come-back))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun perform-escape-through-front-door ()
  (format t ";; Escaping through the front door.~%")
  (setf *mark-safe-p* t))

(defun escape-through-front-door-p (condition)
  (declare (ignore condition))
  (not *front-door-locked-p*))


(defun perform-escape-through-back-door ()
  (format t ";; Escaping through the back door.~%")
  (setf *mark-safe-p* t))

(defun escape-through-back-door-p (condition)
  (declare (ignore condition))
  (not *back-door-locked-p*))

(defun perform-excuse (excuse)
  (format t ";; Mark makes an excuse before leaving:~%;; \"~A\"~%" excuse)
  (setf *mark-safe-p* t))
