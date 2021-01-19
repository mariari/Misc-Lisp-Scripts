
(defpackage #:scripts.akochan
  (:use #:uiop #:inferior-shell)
  (:use #:common-lisp)
  (:export :akochan))

(in-package #:scripts.akochan)

;; The folder contents are as follows
;; ~/.local/lib for
;; .
;; ├── akochan
;; ├── libai.so
;; ├── system.exe
;; └── tactics.json

;; where akochan is the
;; https://github.com/critter-mj/akochan
;; the tactics.json is from
;; https://github.com/Equim-chan/akochan-reviewer

;; the libai.so is from the ai lib inside akochan, and system.exe is
;; the system executable, they should also be in the akochan directory
;; as well

(defun akochan (name dir &rest body)
  (let ((files (uiop:directory-files dir)))
    (mapc
     (lambda (file)
       (let* ((file-string
                (apply #'concatenate 'string (read-file-lines file)))
              (decoded
                (cl-json:decode-json-from-string file-string))
              (pos
                (position name (cdr (assoc :name decoded))
                          :test #'string=)))
         (run `(bash -c
                     ,(concatenate
                       'string
                       ;; we need lib-ai on this path
                       ;; for some reason the linker doesn't work
                       "LD_LIBRARY_PATH=~/.local/lib/akochan \\
                       akochan-reviewer \\
                       --lang en \\
                       -d ~/.local/lib/akochan/akochan/ \\
                       -c ~/.local/lib/akochan/tactics.json "
                       "-a "
                       (format nil "~{~A~^ ~}" body)
                       (or (write-to-string pos) 0)
                       " -i "
                       (namestring file)
                       " -o "
                       (concatenate 'string
                                    (directory-namestring file)
                                    "analysis/"
                                    (file-namestring file)))))))
     files)))
