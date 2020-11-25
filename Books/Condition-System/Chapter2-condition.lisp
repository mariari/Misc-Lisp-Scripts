;; Like chapter 2, however we are starting to play with the condition
;; system


(defvar *phonebook*
  '((:mom :parent)
    (:dad :parent)
    (:alice     :classmate :homework :csgo)
    (:bob       :classmate :homework)
    (:catherine :classmate :ex)
    (:dorthy    :classmate :girlfriend :csgo)
    (:eric      :classmate :homework)
    (:dentist)))

(defvar *csgo-launched-p* nil)

(defun call-person (person)
  (format t ";; Calling ~A.~%" (first person)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call Site
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-people ()
  (setf *csgo-launched-p* nil)
  (dolist (person *phonebook*)
    (catch :do-not-call
      (signal 'before-call :person person)
      (call-person person))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition before-call ()
  ((%person :reader person :initarg :person)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ensure-csgo-launched (condition)
  (let ((person (person condition)))
    (when (member :csgo person)
      (unless *csgo-launched-p*
        (format t ";; Launching Counter Strike for ~A.~%" (first person))
        (setf *csgo-launched-p* t)))))

(handler-bind ((before-call #'ensure-csgo-launched))
  (call-people))

(defun skip-non-csgo-people (condition)
  (let ((person (person condition)))
    (unless (member :csgo person)
      (format t ";; Nope, not calling ~A.~%" (first person))
      (throw :do-not-call nil))))

(handler-bind ((before-call #'ensure-csgo-launched)
               (before-call #'skip-non-csgo-people))
  (call-people))
