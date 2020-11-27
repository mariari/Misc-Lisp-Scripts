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

(defun call-people% ()
  (setf *csgo-launched-p* nil)
  (dolist (person *phonebook*)
    (catch :do-not-call
      (signal 'before-call :person person)
      (call-person person))))

(defun call-people ()
  (setf *csgo-launched-p* nil)
  (dolist (person *phonebook*)
    (catch :do-not-call
      (signal 'before-call :person person)
      (call-person person)
      (signal 'after-call :person person))))


;; first version with signal
(defun receive-phone-call% (person)
  (format t ";; Answering a call from ~A.~%" (first person))
  (when (member :ex person)
    (format t ";; About to commit a grave mistake...~%")
    (signal 'grave-mistake :reason :about-to-call-your-ex)
    (error "we do not want to be here")))

;; Now we try erroring out instead of signaling
(defun receive-phone-call (person)
  (format t ";; Answering a call from ~A.~%" (first person))
  (when (member :ex person)
    (format t ";; About to commit a grave mistake...~%")
    (error 'grave-mistake :reason :about-to-call-your-ex)
    (error "we do not want to be here")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition before-call ()
  ((%person :reader person :initarg :person)))

(define-condition after-call ()
  ((%person :reader person :initarg :person)))

(define-condition grave-mistake (error)
  ((%reason :reader reason :initarg :reason)))


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
  (call-people%))


(defun skip-non-csgo-people (condition)
  (let ((person (person condition)))
    (unless (member :csgo person)
      (format t ";; Nope, not calling ~A.~%" (first person))
      (throw :do-not-call nil))))

(handler-bind ((before-call #'ensure-csgo-launched)
               (before-call #'skip-non-csgo-people))
  (call-people%))


(defun maybe-call-parent (condition)
  (let ((person (person condition)))
    (when (and (zerop (random 2)) (member :parent person))
      (format t ";; Nah, not calling ~A this time.~%" (first person))
      (throw :do-not-call nil))))

(defun skip-non-parents (condition)
  (let ((person (person condition)))
    (unless (member :parent person)
      (throw :do-not-call nil))))

(handler-bind ((before-call #'maybe-call-parent)
               (before-call #'skip-non-parents))
  (call-people%))


(defun skip-ex (condition)
  (let ((person (person condition)))
    (when (member :ex person)
      (throw :do-not-call nil))))

(defun wish-happy-holidays (condition)
  (format t ";; Gonna wish ~A happy holidays!~%" (first (person condition))))


(handler-bind ((before-call #'skip-ex)
               (before-call #'wish-happy-holidays))
  (call-people%))

(handler-bind ((before-call #'skip-ex))
  (handler-bind ((before-call #'wish-happy-holidays))
    (call-people%)))


(defun call-girlfriend-again (condition)
  (let ((person (person condition)))
    (when (member :girlfriend person)
      (format t ";; Gonna call ~A again.~%" (first person))
      (call-person person))))

(handler-bind ((before-call #'ensure-csgo-launched)
               (after-call #'call-girlfriend-again))
  (call-people))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defusing an argument
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun defuse-error (condition)
  (declare (ignore condition))
  (format t ";; Nope nope nope, not answering!~%")
  (throw :do-not-answer nil))

(defun defuse-grave-error (condition)
  (let ((reason (reason condition)))
    (format t ";; Nope nope nope, not answering - reason was, ~A!~%" reason))
  (throw :do-not-answer nil))

(handler-bind ((error #'defuse-error))
  (dolist (person *phonebook*)
    (catch :do-not-answer
      (receive-phone-call person))))

(handler-bind ((grave-mistake #'defuse-grave-error))
  (dolist (person *phonebook*)
    (catch :do-not-answer
      (receive-phone-call person))))
