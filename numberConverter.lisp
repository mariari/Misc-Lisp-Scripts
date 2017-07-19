;; All in 1-------------------------------------------------------------------------------------
(defun convert (num)
  (when (> (length num) 0)
    (flet ((compute (num)
             (cons num (mapcar (lambda (x) (num-to-redix (concatenate 'string x num))) '("0o" "0x" "0y")))))
      (if (/= (r-check num) 10)
          (compute (princ-to-string (redix-to-num num)))
          (compute num)))))

;;; Components-----------------------------------------------------------------------------------
(defun redix-to-num (full-num)
  "Converts a base 2(0y), 8(0o), and 16(0x) number"
  ;; num-list has to be lower case before we pass it in or else the math will add up wrong 
  (let ((num-list (string-to-hexdec-list (string-downcase (subseq full-num 2)))))
    (reduce #'+ (mapcar (lambda (num pos)
                 (* num (expt (r-check full-num) pos))) ; exponent each number by it's position in the list
               num-list
               (range (1- (length num-list)) 0))))) ; (expt ? 0)

(defun num-to-redix (full-num &optional base)
  ;; Grab the 0x/o/y then combine it with the converted string 
  (concatenate 'string
               (subseq full-num 0 2)
               (coerce (mod-number (parse-integer (subseq full-num 2)) ; mod-number returns a list '(#\a #\1)
                                   (r-check full-num base))
                       'string)))
;;; Helper methods-------------------------------------------------------------------------------
(defun string-to-hexdec-list (str)
  "Converts every number of the string into a list... assumes that the list is all lowercase"
  (map 'list (lambda (char)
               (if (digit-char-p char)
                   (digit-char-p char)
                   (- (char-int  char) 87))) str))

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

(defun range (max &optional (min 0) (step 1))
  (flet ((compute (first second)
           (loop for x from first to second by step
              collect x)))
    (if (> min max)
        (compute max min)
        (reverse (compute min max)))))

;;; Useless functions that helped me learn but are now useless-----------------------------------

(defun redix-to-num% (full-num)
  "Converts a base 2(0y), 8(0o), and 16(0x) number"
  ;; num-list has to be lower case before we pass it in or else the math will add up wrong 
  (let* ((num-list (string-to-hexdec-list (string-downcase (subseq full-num 2))))
	 (list-pos (length num-list)))
    (reduce #'+ (mapcar (lambda (num)
                 ;; Reduce list-pos by 1 every iteration so the exponent gives the proper value
                 (decf  list-pos 1)     ; SE
                 (if (= list-pos 0)
                     num
                     (* num (expt (r-check full-num) list-pos))))
               num-list))))

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

;; wanna just cheat-----------------------------------------------------------------------------
(defun convert% (string)
  (flet ((parse (redix)
           (parse-integer string :start 2 :radix redix)))
    (case (char string 1)
      ((#\y) (parse 2))
      ((#\o) (parse 8))
      ((#\x) (parse 16))
      (t     (error (format nil "Illegal base designation in ~a" string))))))

(defun convert%% (string)
  (let ((designators '(("0y" . 2)
                       ("0o" . 8)
                       ("0x" . 16))))
    (cond ((string= "" string) 0)
          ((every #'digit-char-p string)
           (parse-integer string))
          ((> (length string) 2)
           (parse-integer string
                          :start 2
                          :radix (cdr (assoc (subseq string 0 2)
                                             designators
                                             :test #'string=))))
          (t (error (format nil "Illegal base designation in ~a" string))))))

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
