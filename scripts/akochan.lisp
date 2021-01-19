
(defpackage #:scripts.akochan
  (:use #:uiop #:inferior-shell)
  (:use #:common-lisp)
  (:export :akochan
           :make-html-name))

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

(defun make-html-name (file)
  (concatenate 'string
               (directory-namestring file)
               "analysis/"
               (pathname-name file)
               ".html"))

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
                          :test #'string=))
              (output-name
                (make-html-name file)))
         ;; check if the file is already fixed
         ;; if so, then skip it
         (unless (probe-file output-name)
           (run `(bash -c
                       ,(concatenate
                         'string
                         ;; we need lib-ai on this path
                         ;; for some reason the linker doesn't work
                         "LD_LIBRARY_PATH=~/.local/lib/akochan \\
                       akochan-reviewer \\
                       --lang en \\
                       -d ~/.local/lib/akochan/akochan/ \\
                       -c ~/.local/lib/akochan/tactics.json \\
                       -no-open "
                         "-a "
                         (format nil "~{~A~^ ~}" body)
                         (or (write-to-string pos) 0)
                         " -i "
                         (namestring file)
                         " -o "
                         (make-html-name file)))))))
     files)))
