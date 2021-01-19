
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (ql:quickload "inferior-shell")
;;   (asdf:load-system :uiop))


(defpackage #:scripts.cache-fstar
  (:use #:uiop #:inferior-shell)
  (:use #:common-lisp)
  (:export :generate-cache))

(in-package :scripts.cache-fstar)

;; This does not work for ulib sadly, it has many finicky parameters see here
;; https://github.com/FStarLang/FStar/issues/1790
;; for some reason if you qualify the entire file instead of assuming current file, it
;; may fail with errors (looking at you FStar.Seq.Base.fst)
;; , this script does not account for that. Please fix those ones manually,
;; or add custom lisp logic to solve that certain case!!!!!!

(defun generate-cache (starting-file &key (r-limit 5))
  (labels ((rec (current-file tried-list)
             (let* ((tried           (nth-value 1
                                                (run `(fstar.exe ,current-file
                                                                 --cache_checked_modules
                                                                 --record_hints
                                                                 --use_hints
                                                                 --z3rlimit ,r-limit)
                                                     :show t
                                                     :error-output :lines)))
                    ;; maybe-not-exist can be either of these two if something is wrong
                    ;; not exist   ==> (file.checked does not exist)
                    ;; stale check ==> (digest mismatch for file)
                    (maybe-not-exist (last (split-string (car (last tried)))
                                           4)))
               (println tried)
               (cond ((equal (cdr maybe-not-exist) '("does" "not" "exist"))
                      (let ((new-file
                              ;; removed the checked off the file, as it doesn't exist
                              (string-trim ".checked" (car maybe-not-exist))))
                        (rec new-file (cons current-file tried-list))))
                     ((equal (butlast maybe-not-exist) '("(digest" "mismatch" "for"))
                      (let ((new-file
                              (string-trim ")" (car (last maybe-not-exist)))))
                        (rec new-file (cons current-file tried-list))))
                     ;; put some logic here popping off dat list
                     ((null tried-list) nil)
                     (t                 (rec (car tried-list) (cdr tried-list)))))))
    (rec starting-file '())))
