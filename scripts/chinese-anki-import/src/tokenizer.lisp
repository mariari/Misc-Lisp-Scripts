(defpackage #:freq-csv.tokenize
  (:use #:uiop)
  (:use #:common-lisp)
  (:local-nicknames (:f :fset)
                    (:shell :inferior-shell))
  (:export
   :chinese-chars-from-file
   :chinese-trad-chars-from-file))


(in-package #:freq-csv.tokenize)

(defparameter *personal-knowledge-set*
  (f:set
   "了" "爲" "的" "一" "是" "我" "也" "在" "不" "他" "她"
   "說" "要" "沒" "有" "沒有" "都" "多" "大人" "去" "你"
   "人" "到" "嗎" "好" "女人" "但" "但是" "來" "不是" "自己"
   "知道" "什麼" "您" "可以" "一下" "才" "一個" "對" "看" "上"
   "請" "我們" "裡" "跟" "因為" "為" "不會" "會" "那個" "這"
   "王子" "給" "這個" "吃" "誰" "只" "告訴" "最" "大" "一樣"
   "個" "開始" "叫" "回去" "愛" "可愛" "中" "見" "比" "月"
   "〇" "一" "二" "三" "四" "五" "六" "七" "八" "九" "十"
   "死" "太" "錢" "前" "書" "生" "父母" "很" "想" "他們"))

(defparameter *exclusion-set*
  (f:union
   (f:set "，" "。" "　" "：" "、" "﹁" "；" "※" "！" "﹂" "？"
          "." "”" "“" "," "]" "[" "" "…" ":" "\"" "..."
          "à" "ǎ" "á" "ā"
          "ē" "é" "ě" "è"
          "ī" "í" "ǐ" "ì"
          "ō" "ó" "ǒ" "ò"
          "「" "」" "『" "』"
          "Ｋ" "《" "》" "【" "】"
          "○" "‵" "＊")
   *personal-knowledge-set*)
  "extra characters that need to be filtered out")


(defun chinese-chars-from-file (input-file output-file &optional (trad t))
  (if trad
      (chinese-trad-chars-from-file input-file output-file)
      (chinese-simp-chars-from-file input-file)))

(defun chinese-simp-chars-from-file (file)
  "splits a chinese text file into a character string list"
  (separated-chinese-chars-from-string (tokenize-file file)))

(defun chinese-trad-chars-from-file (input-file output-file)
  "splits a chinese text file into a character string list.
However Since the program we use is bad at simplified we must convert
to simplified then back to traditional.  We also have to save it to a
file sadly, so this is an extra side effect that we would rather not deal with."
  (trad-to-simp input-file output-file)
  (save-to-file output-file
                (tokenize-file output-file))
  (simp-to-trad output-file output-file)
  (separated-chinese-chars-from-string (read-string-from-file output-file)))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers needed cause Jieba is bad at Traditional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun trad-to-simp (input-file output-file)
  "uses opencc to convert. Needed because jieba is very bad at
  traditional text"
  (shell:run `(opencc -c tw2sp -i ,input-file -o ,output-file)))

(defun simp-to-trad (input-file output-file)
  "uses opencc to convert."
  (shell:run `(opencc -c s2twp -i ,input-file -o ,output-file)))


(defun read-string-from-file (input-file)
  (alexandria:read-file-into-string input-file))

(defun save-to-file (out-file str)
  (alexandria:write-string-into-file str out-file
                                     :if-exists         :supersede
                                     :if-does-not-exist :create))
