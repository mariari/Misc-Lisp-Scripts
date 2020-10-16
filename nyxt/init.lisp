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


(setf lparallel:*kernel* (lparallel:make-kernel 12))



(define-command mpv-launch (&key annotate-visible-only-p)
  "Show a set of element hints, and go to the user inputted one in the
currently active buffer."
  (nyxt/web-mode::query-hints
   "open video in mpv"
   (lambda (hint)
     (case (type-of hint)
       (nyxt/web-mode::LINK-HINT
        (lparallel:submit-task (lparallel:make-channel)
                               (lambda () (uiop:run-program `("mpv" ,(nyxt:object-string hint))))))
       (t
        (print (type-of hint))
        (print hint))))
   :annotate-visible-only-p annotate-visible-only-p))


(define-command mpv-here ()
  "open the current buffer in mpv"
  (lparallel:submit-task (lparallel:make-channel)
                         (lambda ()
                           (uiop:run-program `("mpv" ,(object-string (url (current-buffer))))))))


;; not used
;; (defvar *custom-keymap* (make-keymap "custom"))

;; (define-key *custom-keymap*
;;   ";x" 'mpv-launch)


(define-mode custom-bind-mode ()
  "Dummy mode for the custom key bindings in `*custom-keymap*'."
  ((keymap-scheme (define-scheme "custom-mode"
                    scheme:vi-normal (list
                                      "; x"   'mpv-launch
                                      "; s-x" 'mpv-launch
                                      "x"     'mpv-here)
                    scheme:emacs     (list
                                      "; x"   'mpv-launch
                                      "; s-x" 'mpv-launch
                                      "x"     'mpv-here)
                    scheme:cua       (list
                                      "; x"   'mpv-launch
                                      "; s-x" 'mpv-launch
                                      "x"     'mpv-here)))))


(define-configuration (buffer web-buffer)
  ((default-modes (append '(custom-bind-mode vi-normal-mode) %slot-default))))
