(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(serapeum babel select)))

(defpackage #:misc.algorithms
  (:documentation "Various Algorithms")
  (:use #:cl #:serapeum)
  (:export :blake2))

(in-package :misc.algorithms)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blake 2 derivation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def iv0 #x6a09e667f3bcc908)   ; (Frac (sqrt 2))
(def iv1 #xbb67ae8584caa73b)   ; (Frac (sqrt 3))
(def iv2 #x3c6ef372fe94f82b)   ; (Frac (sqrt 5))
(def iv3 #xa54ff53a5f1d36f1)   ; (Frac (sqrt 7))
(def iv4 #x510e527fade682d1)   ; (Frac (sqrt 11))
(def iv5 #x9b05688c2b3e6c1f)   ; (Frac (sqrt 13))
(def iv6 #x1f83d9abfb41bd6b)   ; (Frac (sqrt 17))
(def iv7 #x5be0cd19137e2179)   ; (Frac (sqrt 19))

(def iv (make-array 8 :initial-contents (list iv0 iv1 iv2 iv3 iv4 iv5 iv6 iv7))
  "The IV vector of blake2b")

(def σ
  #2A(( 0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15)
      (14 10  4  8  9 15 13  6  1 12  0  2 11  7  5  3)
      (11  8 12  0  5  2 15 13 10 14  3  6  7  1  9  4)
      ( 7  9  3  1 13 12 11 14  2  6  5 10  4  0 15  8)
      ( 9  0  5  7  2  4 10 15 14  1 11 12  6  8  3 13)
      ( 2 12  6 10  0 11  8  3  4 13  7  5 15 14  1  9)
      (12  5  1 15 14 13  4 10  0  7  6  3  9  2  8 11)
      (13 11  7 14 12  1  3  9  5  0 15  4  8  6  2 10)
      ( 6 15 14  9 11  3  0  8 12  2 13  7  1  4 10  5)
      (10  2  8  4  7  6  1  5 15 11  9 14  3 12 13  0))
  "The sigma constant of blake2b")

(defun row-sigma (i &optional (σ σ))
  (select:select σ i t))

#x001010000

(deftype byte-array (&optional x)
  (if x
      `(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (,x))
      `(SIMPLE-ARRAY (UNSIGNED-BYTE 8))))

;; why does the wiki article have so much for mutation
;; This is my slow version.
;; I'm not sure how much the extra conversions really cost us, but I
;; assume quite a bit

(-> blake2 (string &optional fixnum string) t)
(defun blake2 (message &optional (output-length 32) (optkey ""))
  "A blake2b implementation"
  (let ((key (babel:string-to-octets optkey))
        (h   (copy-seq iv)))
    (setf (aref h 0)
          (logxor (aref h 0)
                  ;; ash 8 = bit shift it by 00 in hex
                  (+ #x01010000 (ash (length key) 8) output-length)))
    (mvlet* ((message (babel:string-to-octets message))
             (pad-amt (- 128 (mod (length message) 128))) ; the amount padded
             (message (pad-to-nearest-multiple message 128))
             (message (if (zerop (length key))
                          message
                          (concatenate 'byte-array (pad key 128) message)))
             (m-rows  (/ (length message) 128))
             ;; reshape array to be a nice x by 128 array
             (message   (reshape (list m-rows 128) message))
             (byts-comp h
                        (mvfold (lambda (bytes-compressed h array-chunk)
                                  (let ((chunk (select:select message array-chunk t)))
                                    (values
                                     (+ bytes-compressed 128)
                                     (compress h chunk bytes-compressed :final nil))))
                                (alexandria:iota (1- m-rows))
                                0 h)))
      ;; TODO truncate the answer back, and convert it to a stirng
      (compress h (select:select message (1- m-rows) t) (+ pad-amt byts-comp) :final t))))

(-> compress (array byte-array fixnum &key (:final boolean)) t)
(defun compress (h chunk bytes-so-far &key (final nil))
  h chunk bytes-so-far final
  h)

(defun pad-to-nearest-multiple (array number)
  "Padds the array to the nearest multiple of number"
  (mvlet* ((arr-length   (length array))
           (multiple rem (floor arr-length number)))
    (if (and (zerop rem) (not (zerop arr-length)))
        array
        (pad array (* (1+ multiple) number)))))

(defgeneric pad (stream number)
  (:documentation "pads the stream by the requested number of elements"))

(defmethod pad ((arr simple-array) number)
  (pad-end arr number 0))

(defmethod pad ((string string) number)
  (let ((length string))
    (if (>= number length)
        string
        (make-string (- number length) :initial-element #\0))))

