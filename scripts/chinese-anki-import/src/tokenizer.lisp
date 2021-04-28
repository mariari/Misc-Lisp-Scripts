(defpackage #:freq-csv.tokenize
  (:use #:uiop)
  (:use #:common-lisp)
  (:local-nicknames (:f :fset)
                    (:shell :inferior-shell))
  (:export
   :chinese-chars-from-file))


(in-package #:freq-csv.tokenize)

(defparameter *exclusion-set*
  (f:set "，" "。" "　" "：" "、" "﹁" "；" "※" "！" "﹂" "？"
         "." "”" "“" "," "]" "[" "" "…" ":" "\"" "..."
         "à" "ǎ" "á" "ā"
         "ē" "é" "ě" "è"
         "ī" "í" "ǐ" "ì"
         "ō" "ó" "ǒ" "ò"
         "了" "爲")
  "extra characters that need to be filtered out")

(defun chinese-chars-from-file (file)
  "splits a chinese text file into a character string list"
  (separated-chinese-chars-from-string (tokenize-file file)))


(declaim (ftype (function ((or pathname string)) (or string t)) tokenize-file))
(defun tokenize-file (input-file)
  "runs jieba on a file, giving us back a string of separated
characters"
  (shell:run/ss `(python -m jieba -d " " ,input-file)))

(defun remove-spaces (string)
  (substitute #\Space #\Newline string))

(defun separated-chinese-chars-from-string (string)
  (remove-if (lambda (str)
               ;; punctuation still exists, and what not
               (or (emptyp str) (f:member? str *exclusion-set*)))
             ;; + gets rid of the empty strings, a-z gets rid of english
             (cl-ppcre:split "( )+|([A-z])+" (remove-spaces string))))

