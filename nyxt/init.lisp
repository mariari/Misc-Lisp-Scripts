#-quicklisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init))))

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
  (uiop:launch-program (list "mpv" link) :ignore-error-status t))

(define-command mpv-launch ()
  "Show a set of element hints, and go to the user inputted one in the
currently active buffer."
  (nyxt/hint-mode:query-hints
   "open video in mpv"
   (lambda (hint)
     (let ((hint (if (listp hint) (car hint) hint)))
       (echo "~A" hint)
       (case (type-of hint)
         (nyxt/dom:a-element
          (echo "MPV launched with ~a" (url hint))
          (execute-mpv (quri:render-uri (url hint))))
         (t
          (echo "failed to launch mpv")
          (print (type-of hint))
          (print hint)))))))

(define-command mpv-here ()
  "executes mpv in the current buffer"
  (execute-mpv (quri:render-uri (url (current-buffer)))))


;; TODO ∷ figure out how to make text spawn if things fail
(define-command mpv-url (&key (prefill-current-url-p t))
  "open an url in mpv"
  (let ((history (set-url-history *browser*)))
    (when history
      (containers:insert-item history (url (current-buffer))))
    (flet ((func (url)
             (let ((url-string
                     (cond ((typep url 'history-entry) (render-url (url url)))
                           ((stringp url)              url)
                           ((valid-url-p url)          (render-url url))
                           (t                          url))))
               (echo "MPV launched with ~a" url)
               (execute-mpv url-string))))
      (prompt
       :prompt (format nil "Launch mpv on")
       :input (if prefill-current-url-p
                  (quri:render-uri (url (current-buffer))) "")
       :sources (list
                 (make-instance 'prompter:raw-source
                                :name "New URL"
                                :actions (list #'func))
                 (make-instance 'global-history-source
                                :actions (list #'func)))
       :history history))))



(defun new-buffer-load (suggestion-value)
  "Load a URL in a new buffer."
  (make-buffer-focus :url (url suggestion-value)))

(defun custom-search-url (&key
                            (prompt-name "")
                            (search-form #'identity)
                            (buffer-load #'new-buffer-load))
  (labels ((url-function (urls)
             ;; url is of type new-url-query
             (let ((url (car urls)))
               (format t "~A" url)
               (funcall buffer-load (funcall search-form url)))))
    (let ((history (set-url-history *browser*)))
      (pushnew-url-history history (url (current-buffer)))
      (prompt
       :prompt prompt-name
       :input ""
       :history history
       :sources (url-sources (current-buffer) (list #'url-function))))))

(defun mdbgt-url (f)
  (labels ((append-mdbgt (url)
             (make-instance 'new-url-query
                            :query (str:concat "https://www.mdbg.net/chinese/dictionary?page=worddict&wdrst=1&wdqb="
                                               (query url)))))
    (custom-search-url :prompt-name "MDGBT search"
                       :search-form #'append-mdbgt
                       :buffer-load f)))

(defun jukuu-url (f)
  (labels ((append-jukuu (url)
             (make-instance 'new-url-query
                            :query (str:concat "http://jukuu.com/search.php?q="
                                               (query url)))))
    (custom-search-url :prompt-name "句酷 search"
                       :search-form #'append-jukuu
                       :buffer-load f)))

(define-command jukuu-set-buffer ()
  "Set the URL for the current buffer, completing with history."
  (jukuu-url #'buffer-load))

(define-command jukuu-new-buffer ()
  "Prompt for a URL and set it in a new focused buffer."
  (jukuu-url #'new-buffer-load))

(define-command mdbgt-set-buffer ()
  "Set the URL for the current buffer, completing with history."
  (mdbgt-url #'buffer-load))

(define-command mdbgt-new-buffer ()
  "Prompt for a URL and set it in a new focused buffer."
  (mdbgt-url #'new-buffer-load))

;; not used
(defvar *custom-keymap* (make-keymap "custom"))

(define-key *custom-keymap*
  ";x" 'mpv-launch)


;; (define-mode custom-bind-mode ()
;;   "Dummy mode for the custom key bindings in `*custom-keymap*'."
;;   ((keymap-scheme (define-scheme "custom-mode"
;;                     scheme:vi-normal (list
;;                                       "; x" 'mpv-launch
;;                                       "X"   'mpv-url
;;                                       "x"   'mpv-here
;;                                       "y f" 'nyxt/web-mode:copy-hint-url
;;                                       "P"   'mdbgt-new-buffer
;;                                       "p"   'mdbgt-set-buffer
;;                                       "\\"  'jukuu-set-buffer
;;                                       "|"   'jukuu-new-buffer)
;;                     scheme:emacs     (list
;;                                       "C-x ; x" 'mpv-launch
;;                                       "C-x X"   'mpv-url
;;                                       "C-x x"   'mpv-here
;;                                       "C-x y f" 'nyxt/web-mode:copy-hint-url
;;                                       "C-x P"   'mdbgt-new-buffer
;;                                       "C-x p"   'mdbgt-set-buffer
;;                                       "C-x \\"  'jukuu-set-buffer
;;                                       "C-x |"   'jukuu-new-buffer)
;;                     scheme:cua       (list)))))

;; (define-mode prompt-buffer-extra-keys ()
;;   "Dummy mode for the custom key bindings in `*custom-keymap*'."
;;   ((keymap-scheme (define-scheme "custom-mode"
;;                     scheme:emacs (list
;;                                   ;; "C-a" 'nyxt/prompt-buffer-mode:select-first
;;                                   ;; "C-e" 'nyxt/prompt-buffer-mode:select-last
;;                                   ;; "M-a" 'nyxt/prompt-buffer-mode:mark-all
;;                                   )
;;                     scheme:cua   (list
;;                                   ;; "C-a" 'nyxt/prompt-buffer-mode:select-first
;;                                   ;; "C-e" 'nyxt/prompt-buffer-mode:select-last
;;                                   ;; "M-a" 'nyxt/prompt-buffer-mode:mark-all
;;                                   )))))


(define-configuration nyxt/search-buffer-mode:search-buffer-source
  ((nyxt/search-buffer-mode:minimum-search-length 1)))

(define-configuration buffer
  ((default-modes %slot-default%)))

(define-configuration browser
    ((default-modes (list* 'prompt-buffer-extra-keys 'emacs-mode 'prompt-buffer-mode %slot-default%))
      (external-editor-program "emacs")))

(defmethod customize-instance ((buffer web-buffer) &key)
  (nyxt/emacs-mode:emacs-mode :buffer buffer))

(define-configuration (buffer web-buffer)
  ((default-modes
    (list* 'emacs-mode 'custom-bind-mode %slot-default%))))

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
