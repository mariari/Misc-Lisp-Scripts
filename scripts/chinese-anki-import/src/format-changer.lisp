(defpackage #:freq-csv.format-changer
  (:use #:uiop)
  (:use #:common-lisp)
  (:local-nicknames (:shell :inferior-shell))
  (:export
   :epub-to-text-file
   :pdf-to-text-file))

(in-package :freq-csv.format-changer)


(defun epub-to-text-file (input output)
  "converts a epub file into a text file"
  (shell:run `(ebook-convert ,input ,output)))

(defun pdf-to-text-file (input output)
  "converts a pdf file into a text file"
  (shell:run `(pdftotext ,input ,output)))
