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
  (uiop:run-program (list "mpv" link) :ignore-error-status t))

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
  "executes mpv in the current buffer"
  (execute-mpv (object-string (url (current-buffer)))))


;; TODO ∷ figure out how to make text spawn if things fail
(define-command mpv-url (&key (prefill-current-url-p t))
  "open an url in mpv"
  (let ((history (set-url-history *browser*)))
    (when history
      (containers:insert-item history (url (current-buffer))))
    (flet ((func (url)
             (let ((url-string
                     (cond ((typep url 'history-entry) (object-string (url url)))
                           ((stringp url)              url)
                           ((valid-url-p url)          (object-string url))
                           (t                          (object-string url)))))
               (execute-mpv url-string))))
      (prompt
       :prompt        (format nil "Launch mpv on")
       :input         (if prefill-current-url-p
                          (object-string (url (current-buffer))) "")
       :sources       (list
                       (make-instance 'prompter:raw-source
                                      :name "New URL"
                                      :actions (list (make-unmapped-command func)))
                       (make-instance 'global-history-source
                                      :actions (list (make-unmapped-command func))))
       :history      history))))



(defun mdbgt-url (f)
  (flet ((mdgb-url (url)
           (funcall f (concatenate 'string "!mdbgt " url))))
    (let ((history (set-url-history *browser*)))
      (when history
        (containers:insert-item history (quri:render-uri (url (current-buffer)))))
      (prompt
       :prompt "MDGBT search"
       :input ""
       :history history
       :sources (list (make-instance 'prompter:raw-source
                                     :name "new MDGB search"
                                     :actions (list (make-unmapped-command mdgb-url))))))))

(define-command mdbgt-set-buffer ()
  "Set the URL for the current buffer, completing with history."
  (mdbgt-url #'buffer-load))

(define-command mdbgt-new-buffer ()
  "Prompt for a URL and set it in a new focused buffer."
  (mdbgt-url #'new-buffer-load))

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
                                      "x"   'mpv-here
                                      "y f" 'nyxt/web-mode:copy-hint-url
                                      "P"   'mdbgt-new-buffer
                                      "p"   'mdbgt-set-buffer)
                    scheme:emacs     (list
                                      "; x" 'mpv-launch
                                      "X"   'mpv-url
                                      "x"   'mpv-here
                                      "y f"  'nyxt/web-mode:copy-hint-url)
                    scheme:cua       (list
                                      "; x" 'mpv-launch
                                      "X"   'mpv-url
                                      "x"   'mpv-here
                                      "y f"  'nyxt/web-mode:copy-hint-url)))))


(define-configuration (buffer web-buffer)
  ((default-modes
    (list* 'custom-bind-mode 'vi-normal-mode 'blocker-mode %slot-default))))

;;;; Presentation

;; (nyxt:buffer-load "https://www.youtube.com/watch?v=BXmOlCy0oBM"
;;                   :buffer (nyxt:make-buffer-focus))
;; (nyxt:buffer-load "https://www.youtube.com/results?search_query=erlang+the+movie"
;;                   :buffer (nyxt:make-buffer-focus))
;; (nyxt:buffer-load "https://nyxt.atlas.engineer/"
;;                   :buffer (nyxt:make-buffer-focus))


;; Download files

;; (defparameter *links*
;;   (butlast (nyxt/web-mode::elements-from-json (nyxt/web-mode::add-element-hints))))

;; (defun grab-audio% (name)
;;   (let ((link (url (car (nyxt/web-mode::elements-from-json (nyxt/web-mode::add-element-hints))))))
;;     (inferior-shell:run
;;      `(curl -o ,name ,link))))

;; (mapc (lambda (link)
;;         (buffer-load (url link))
;;         (let* ((name (NYXT/WEB-MODE::BODY link))
;;                (link (concatenate 'string (subseq name 0 (- (length name) 3)) ".mp3")))
;;           (print link)
;;           (grab-audio% link)))
;;       *links*)

;; (mapcar (lambda (link)
;;           (buffer-load (url link))
;;           (let* ((name (NYXT/WEB-MODE::BODY link))
;;                  (link (concatenate 'string (subseq name 0 (- (length name) 3)) ".mp3")))
;;             (list link (url (car (nyxt/web-mode::elements-from-json (nyxt/web-mode::add-element-hints)))))))
;;  *links*)
