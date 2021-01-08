;; This is the section before restarts
;; A prelude in how to implement said system

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *mark-safe-p* nil)
(defvar *front-door-locked-p* nil)
(defvar *back-door-locked-p* nil)

(defvar *choices* '())

(defvar *excuses*
  '("Kate did not divide her program into sections properly!"
    "I was borrowing Kate's books on mainframe programming!"
    "I had COBOL-related homework and hoped Kate could help me!"))

(defstruct choice
  (name            (error "Must provide :NAME."))
  (effect-function (error "Must provide :EFFECT-FUNCTION."))
  (test-function   (constantly t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun try-to-hide-mark% ()
  (let ((choices (compute-choices)))
    (if choices
        (let ((choice (first choices)))
          (format t ";; Performing ~A.~%" (choice-name choice))
          (funcall (choice-effect-function choice)))
        (format t ";; Kate cannot hide Mark!~%"))))

(defun parents-come-back% ()
  (format t ";; Uh oh - Kate's parents are back!~%")
  (try-to-hide-mark%)
  (if *mark-safe-p*
      (format t ";; Whew... We're safe! for now.~%")
      (error "we do not want to be here")))


(defun try-to-hide-mark ()
  (let ((choice (find-choice 'escape)))
    (cond (choice
           (invoke-choice choice))
          (t
           (format t ";; Kate cannot hide Mark!~%")
           (let ((excuse (find-choice 'excuse)))
             (when excuse
               (let ((excuse-text (elt *excuses* (random (length *excuses*)))))
                 (invoke-choice excuse excuse-text))))))))

(defun parents-come-back ()
  (format t ";; Uh oh - Kate's parents are back!~%")
  (try-to-hide-mark)
  (if *mark-safe-p*
      (format t ";; Whew... We're safe! for now.~%")
      (error "we do not want to be here")))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main helpers
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compute-choices ()
  (loop for choice in *choices*
        when (funcall (choice-test-function choice))
          collect choice))

(defun find-choice (name)
  (loop for choice in *choices*
        when (and (eq name (choice-name choice))
                (funcall (choice-test-function choice)))
          return choice))

(defun invoke-choice (name &rest arguments)
  (apply (choice-effect-function name)
         arguments))

(defun perform-excuse (excuse)
  (format t ";; Mark makes an excuse before leaving:~%;; \"~A\"~%" excuse)
  (setf *mark-safe-p* t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Escape hatches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *front-door-choice*
  (make-choice
   :name 'escape-through-front-door
   :effect-function #'perform-escape-through-front-door
   :test-function #'escape-through-front-door-p))

(defvar *back-door-choice*
  (make-choice
   :name 'escape-through-back-door
   :effect-function #'perform-escape-through-back-door
   :test-function #'escape-through-back-door-p))

(defun call-with-home-choices% (thunk)
  (let ((*choices* (list *front-door-choice* *back-door-choice*)))
    (funcall thunk)))

(call-with-home-choices%
 (lambda ()
   (let ((*mark-safe-p* nil)
         (*front-door-locked-p* nil)
         (*back-door-locked-p* nil))
     (parents-come-back%))))


;; Now add proper naming conventions
(defun call-with-home-choices (thunk)
  (let ((*choices*
          (list (make-choice
                 :name 'escape
                 :effect-function #'perform-escape-through-front-door
                 :test-function #'escape-through-front-door-p)
                (make-choice
                 :name 'escape
                 :effect-function #'perform-escape-through-back-door
                 :test-function #'escape-through-back-door-p)
                (make-choice
                 :name 'excuse
                 :effect-function #'perform-excuse))))
    (funcall thunk)))

(call-with-home-choices
 (lambda ()
   (let ((*mark-safe-p* nil)
         (*front-door-locked-p* t)
         (*back-door-locked-p* nil))
     (parents-come-back))))

(call-with-home-choices
 (lambda ()
   (let ((*mark-safe-p* nil)
         (*front-door-locked-p* t)
         (*back-door-locked-p* t))
     (try-to-hide-mark))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun perform-escape-through-front-door ()
  (format t ";; Escaping through the front door.~%")
  (setf *mark-safe-p* t))

(defun escape-through-front-door-p ()
  (format t ";; The front door is~:[ not~;~] locked.~%" *front-door-locked-p*)
  (not *front-door-locked-p*))


(defun perform-escape-through-back-door ()
  (format t ";; Escaping through the back door.~%")
  (setf *mark-safe-p* t))

(defun escape-through-back-door-p ()
  (format t ";; The back door is~:[ not~;~] locked.~%" *back-door-locked-p*)
  (not *back-door-locked-p*))
