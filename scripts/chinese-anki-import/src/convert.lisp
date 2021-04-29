(defpackage #:freq-csv.convert
  (:use #:common-lisp)
  (:local-nicknames (:f        :fset)
                    (:tokenize :freq-csv.tokenize)
                    (:format   :freq-csv.format-changer))
  (:export
   :text-file-to-frequency-csv
   :pdf-file-to-frequency-csv
   :epub-file-to-frequency-csv))


(in-package #:freq-csv.convert)

(defun pdf-file-to-frequency-csv (in-file out-file &key ignore-below (trad t))
  (format:pdf-to-text-file in-file out-file)
  (text-file-to-frequency-csv out-file out-file
                              :ignore-below ignore-below
                              :trad trad))

(defun epub-file-to-frequency-csv (in-file out-file &key ignore-below (trad t))
  (format:epub-to-text-file in-file out-file)
  (text-file-to-frequency-csv out-file out-file
                              :ignore-below ignore-below
                              :trad trad))


(defun text-file-to-frequency-csv (in-file out-file &key ignore-below (trad t))
  "takes a text file filled with chinese characters and produces a
frequency list of the characters"
  (let ((alist (bag-to-alist
                (radical-list-to-bag
                 (tokenize:chinese-chars-from-file in-file out-file trad)))))
    (alist-to-csv (sort-alist (if ignore-below
                                  (remove-less-than-alist alist ignore-below)
                                  alist))
                  out-file)))

(defun alist-to-csv (alist out-file)
  (with-open-file (stream out-file
                          :direction         :output
                          :if-exists         :supersede
                          :if-does-not-exist :create)
    (mapc (lambda (c)
            (format stream "~a; ~a~%" (car c) (cdr c)))
          alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bag helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun radical-list-to-bag (list-o-radicals)
  "converts a character list to a bag"
  (reduce #'f:with list-o-radicals :initial-value (f:empty-bag)))

(defun bag-to-alist (bag)
  (let ((alist nil))
    (f:do-bag-pairs (ele occurences bag)
      (setf alist (cons (cons ele occurences) alist)))
    alist))

(defun sort-alist (list)
  (sort list #'> :key #'cdr))


(defun remove-less-than-alist (alist less-than)
  "removes all elements in the alist for the value"
  (remove-if (lambda (x) (< x less-than)) alist :key #'cdr))
;; (defun process-text-file (file))

;; (sort-alist (bag-to-alist *y*))

;; (radical-list-to-bag *x*)

;; (defparameter *x* (tokenize:chinese-chars-from-file #P"test/input.txt"))
