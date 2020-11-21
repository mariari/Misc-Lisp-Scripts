(ql:quickload :slynk)

(in-package :nyxt)

(when (asdf:load-system :slynk)
  (define-command start-slynk (&optional (slynk-port *swank-port*))
    "Start a Slynk server that can be connected to, for instance, in
Emacs via SLY.

Warning: This allows Nyxt to be controlled remotely, that is, to
execute arbitrary code with the privileges of the user running Nyxt.
Make sure you understand the security risks associated with this
before running this command."
      (slynk:create-server :port slynk-port :dont-close t)
      (echo "Slynk server started at port ~a" slynk-port)))


;; (setf lparallel:*kernel* (lparallel:make-kernel 12))

(defun execute-mpv (link)
  (chanl:pexec ()
    (uiop:run-program (list "mpv" link) :ignore-error-status t)))



(define-command mpv-launch (&key annotate-visible-only-p)
  "Show a set of element hints, and go to the user inputted one in the
currently active buffer."
  (nyxt/web-mode::query-hints
   "open video in mpv"
   (lambda (hint)
     (case (type-of hint)
       (nyxt/web-mode::link-hint
        (execute-mpv (nyxt:object-string hint)))
       (t
        (print (type-of hint))
        (print hint))))
   :annotate-visible-only-p annotate-visible-only-p))


(define-command mpv-here ()
  "open the current buffer in mpv"
  (execute-mpv (object-string (url (current-buffer)))))

;; TODO ∷ figure out how to make text spawn if things fail
(define-command mpv-url (&key prefill-current-url-p)
  "open an url in mpv"
  (let ((history (minibuffer-set-url-history *browser*)))
    (when history
      (containers:insert-item history (url (current-buffer))))
    (let* ((url (prompt-minibuffer
                 :input-prompt        (format nil "Launch mpv on")
                 :input-buffer        (if prefill-current-url-p
                                          (object-string (url (current-buffer))) "")
                 :default-modes      '(set-url-mode minibuffer-mode)
                 :suggestion-function (history-suggestion-filter
                                       :prefix-urls (list (object-string
                                                           (url (current-buffer)))))
                 :history      history
                 :must-match-p nil))
          (url-string
            (cond ((typep url 'history-entry) (object-string (url url)))
                  ((stringp url)              url)
                  (t                          (format nil "error: url on mpv-url is ~a" url)))))
      (execute-mpv url-string))))


;; not used
;; (defvar *custom-keymap* (make-keymap "custom"))

;; (define-key *custom-keymap*
;;   ";x" 'mpv-launch)


(define-mode custom-bind-mode ()
  "Dummy mode for the custom key bindings in `*custom-keymap*'."
  ((keymap-scheme (define-scheme "custom-mode"
                    scheme:vi-normal (list
                                      "; x" 'mpv-launch
                                      "X"   'mpv-url
                                      "x"   'mpv-here)
                    scheme:emacs     (list
                                      "; x" 'mpv-launch
                                      "X"   'mpv-url
                                      "x"   'mpv-here)
                    scheme:cua       (list
                                      "; x" 'mpv-launch
                                      "X"   'mpv-url
                                      "x"   'mpv-here)))))


(define-configuration (buffer web-buffer)
  ((default-modes
    (list* 'custom-bind-mode 'vi-normal-mode 'blocker-mode %slot-default))))
