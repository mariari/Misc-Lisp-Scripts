

(defvar *hooks* '())

(defvar *before-hooks* '())

(defvar *after-hooks* '())

(defvar *csgo-launched-p* nil)

(defvar *phonebook*
  '((:mom :parent)
    (:dad :parent)
    (:alice     :classmate :homework :csgo)
    (:bob       :classmate :homework)
    (:catherine :classmate :ex)
    (:dorthy    :classmate :girlfriend :csgo)
    (:eric      :classmate :homework)
    (:dentist)))


(defun call-person (person)
  (format t ";; Calling ~A.~%" (first person)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call Site
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Version with just *hooks*
(defun call-people% ()
  (setf *csgo-launched-p* nil)
  (dolist (person *phonebook*)
    (catch :do-not-call
      (dolist (hook *hooks*)
        (funcall hook person))
      (call-person person))))


;; Version with before and after *hooks*
(defun call-people ()
  ;; I had to abstract from the original code instead of 2 separate
  ;; dotimes just use mapc and abstract from the call site
  (setf *csgo-launched-p* nil)
  (dolist (person *phonebook*)
    (flet ((call-hooks (xs) (mapc (lambda (f) (funcall f person)) xs)))
      (catch :do-not-call
        (call-hooks *before-hooks*)
        (call-person person)
        (call-hooks *after-hooks*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks Original
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-non-csgo-person (person)
  (unless (member :csgo person)
    (format t ";; Nope, not calling ~A.~%" (first person))
    (throw :do-not-call nil)))

(defun ensure-csgo-launched (person)
  (when (member :csgo person)
    (unless *csgo-launched-p*
      (format t ";; Launching Counter Strike for ~A.~%" (first person))
      (setf *csgo-launched-p* t))))


(let ((*hooks* (list #'skip-non-csgo-person #'ensure-csgo-launched)))
  (call-people%))


(defun maybe-call-parent (person)
  (when (member :parent person)
    (when (zerop (random 2))
      (format t ";; Nah, not calling ~A this time.~%" (first person))
      (throw :do-not-call nil))))

(defun skip-non-parents (person)
  (unless (member :parent person)
    (throw :do-not-call nil)))

(let ((*hooks* (list #'maybe-call-parent #'skip-non-parents)))
  (call-people%))


(defun skip-ex (person)
  (when (member :ex person)
    (throw :do-not-call nil)))

(defun wish-happy-holidays (person)
  (format t";; Gonna wish ~A happy holidays!~%" (first person)))

(let ((*hooks* (list #'wish-happy-holidays)))
  (let ((*hooks* (cons #'skip-ex *hooks*)))
    (call-people%)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hooks Before and After
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun call-girlfriend-again (person)
  (when (member :girlfriend person)
    (format t ";; Gonna call ~A again.~%" (first person))
    (call-person person)))

(let ((*before-hooks* (list #'ensure-csgo-launched))
      (*after-hooks*  (list #'call-girlfriend-again)))
  (call-people))
