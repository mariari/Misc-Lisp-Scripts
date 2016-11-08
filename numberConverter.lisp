;; All in 1-------------------------------------------------------------------------------------
(defun convert (num)
  (when (> (length num) 0)
    (when (/= (r-check num) 10)
      (setf num (princ-to-string (redix-to-num num))))
    (cons num (mapcar (lambda (x) (num-to-redix (concatenate 'string x num))) '("0o" "0x" "0y")))))

;;; Components-----------------------------------------------------------------------------------
(defun redix-to-num (full-num)
  "Converts a base 2(0y), 8(0o), and 16(0x) number"
  ;; num-list has to be lower case before we pass it in or else the math will add up wrong 
  (let* ((num-list (string-to-hexdec-list (string-downcase (subseq full-num 2))))
	 (list-pos  (length num-list)))
    (reduce #'+ (mapcar (lambda (num)
                 ;; Reduce list-pos by 1 every iteration so the exponent gives the proper value...SE
                 (decf  list-pos 1)
                 (if (= list-pos 0)
                     num
                     (* (expt (r-check full-num) list-pos) num)))
               num-list))))

(defun num-to-redix (full-num &optional base)
  ;; Grab the 0x/o/y then combine it with the converted string 
  (concatenate 'string (subseq full-num 0 2) ; mod-number returns a list '(#\a #\1) which gets coerced   
               (coerce (mod-number (parse-integer (subseq full-num 2))
                                   (r-check full-num base))
                       'string)))
;;; Helper methods-------------------------------------------------------------------------------
(defun string-to-hexdec-list (str)
  "Converts every number of the string into a list... assumes that the list is all lowercase"
  (map 'list (lambda (char)
               (if (digit-char-p char)
                   (digit-char-p char)
                   (- (char-int char) 87))) str))

(defun mod-number (num redix)
  "Returns a list of characters from moding the number given"
  (labels ((f (return numbers)
             (if (= numbers 0)
                 return
                 (let ((mod-num (mod numbers redix)))
                   (f (cons (if (> mod-num 9)
                                ;;  Offset of 87 Makes 10 an a and so on 
                                (code-char (+ 87 mod-num))
                                (digit-char      mod-num)) ; we make this a char so we can coerce it 
                            return)
                      (floor numbers redix))))))
    (f nil num)))

(defun r-check (str &optional (base 10))
  (if (or (> (length str) 1) (= base 10))
      (case (char str 1)
        ((#\y) 2)
        ((#\o) 8)
        ((#\x) 16)
        (t     10))
      base))

;;; Useless functions that helped me learn but are now useless-----------------------------------
(defun string-to-hexdec-list% (str)
  "Converts every number of the string into a list... assumes that the list is all lowercase" 
  (labels ((transform (lst string)
             (if (= (length string) 0)
                 lst               ;Checks if it is a number or char
                 (transform (cons (if (digit-char-p (char string 0))
                                      (digit-char-p (char string 0))  
                                      (- (char-int (char string 0)) 87))
                                  lst)
                            (subseq string 1)))))
    (reverse (transform nil str))))      ; reverse output is still more efficient then using append! 

(defun hexdec-list-to-string% (list)
  "Converts a list into a string with no spaces"
  (remove #\Space (string-trim '(#\( #\)) (princ-to-string list)))) ; turns out using coerce on chars is better than this on numbers and chars 

;; (defun convert% (num)
;;   (when (case (length num)
;;           (1 t)
;;           (0 nil)
;;           (t (if (/= (r-check num) 10)
;;                  (setf num (princ-to-string (redix-to-num num))) t)))
;;     (format t "~a      "  num)
;;     (mapcar (lambda (x) (format t "~a      " 
;;                          (num-to-redix (concatenate 'string x num)))) '("0o" "0x" "0y"))))


;; (defun string-to-hexdec-list (str)
;;   "Converts every number of the string into a list... assumes that the list is all lowercase"
;;   (map 'list (lambda (char)
;;                (<*> ((lambda (x y z) (if (digit-char-p x) y z)))
;;                     (digit-char-p) char
;;                     (- (char-int char) 87))) str))

;; (defun convert (num)
;;   (when (> (length num) 0)
;;     (when (/= (r-check num) 10)
;;       (setf num (princ-to-string (redix-to-num num))))
;;     (<*> (cons) ((lambda (y) (mapcar (lambda (x) (num-to-redix (concatenate 'string x y))) '("0o" "0x" "0y")))) num)))
