;; split this out into multiple files plz

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(serapeum babel select cl-intbytes bit-smasher cl-utilities array-utils fiveam)))

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

(deftype u64-byte-array (&optional len)
  (if len
      `(SIMPLE-ARRAY (UNSIGNED-BYTE 64) (,len))
      `(SIMPLE-ARRAY (UNSIGNED-BYTE 64))))

(deftype byte-array (&optional x)
  (if x
      `(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (,x))
      `(SIMPLE-ARRAY (UNSIGNED-BYTE 8))))


(deftype pos-fixnum () `(alexandria:positive-fixnum))

(declaim (type (u64-byte-array 8) iv))
(def iv (make-array 8
                    :initial-contents (list iv0 iv1 iv2 iv3 iv4 iv5 iv6 iv7)
                    :element-type    '(unsigned-byte 64))
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

(defun row (i σ)
  (select:select σ i t))

#x001010000

;; why does the wiki article have so much for mutation
;; This is my slow version.
;; I'm not sure how much the extra conversions really cost us, but I
;; assume quite a bit

(locally (declare (optimize (speed 3) (safety 0))))

;; Debug what is wrong after 127

(-> blake2 (string &optional fixnum string) t)
(defun blake2 (message &optional (output-length 64) (optkey ""))
  "A blake2b implementation"
  (let* ((message     (babel:string-to-octets message))
         (key         (babel:string-to-octets optkey))
         (h           (copy-seq iv))
         (pad-key-len (ash (length key) 8))) ; ash 8 = bit shift it by 00 in hex
    (declare (byte-array key)     (fixnum pad-key-len)
             (byte-array message) ((u64-byte-array 8) h))

    (setf (aref h 0)
          (logxor (aref h 0)
                  (the pos-fixnum (+ #x01010000 pad-key-len output-length))))

    (mvlet* ((left-over   (mod (length message) 128)) ; the amount padded
             (message     (pad-to-nearest-multiple message 128))
             (message-key (if (zerop (length key))
                              message
                              (concatenate 'byte-array (pad key 128) message)))
             (m-rows      (/ (length message-key) 128))
             (2d-msg      (reshape (list m-rows 128) message-key)) ; a nice x by 128 array
             (byts-comp h (mvfold (lambda (bytes-compressed h array-chunk)
                                    (let ((chunk (select:select 2d-msg array-chunk t)))
                                      (values
                                       #1=(+ bytes-compressed 128)
                                       (compress h chunk #1# :final nil))))
                                  (alexandria:iota (1- m-rows))
                                  0 h))
             (total-bytes (+ left-over byts-comp)))
      (declare (fixnum m-rows) (byte-array message) (byte-array message-key))
      ;; TODO truncate the answer back, and convert it to a stirng
      (proper-endian
       (compress h (select:select 2d-msg (1- m-rows) t) total-bytes :final t)))))

(-> compress ((u64-byte-array 8) byte-array (unsigned-byte 128) &key (:final boolean)) t)
(defun compress (h chunk bytes-so-far &key (final nil))
  (let ((v (concatenate '(u64-byte-array 16) h iv))
        (m (byte-array-to-64-bit-word-array chunk)))
    (declare ((u64-byte-array 16) m))
    (setf (aref v 12) (logxor (aref v 12) (lower-64bits bytes-so-far)))
    (setf (aref v 13) (logxor (aref v 13) (upper-64bits bytes-so-far)))
    (when final
      (setf (aref v 14) (logxor (aref v 14) #xFFFFFFFFFFFFFFFF)))

    (dotimes (i 12)
      (let ((sigma (row (mod i 10) σ)))
        (mix v 0 4 8  12 (aref m (aref sigma 0)) (aref m (aref sigma 1)))
        (mix v 1 5 9  13 (aref m (aref sigma 2)) (aref m (aref sigma 3)))
        (mix v 2 6 10 14 (aref m (aref sigma 4)) (aref m (aref sigma 5)))
        (mix v 3 7 11 15 (aref m (aref sigma 6)) (aref m (aref sigma 7)))

        (mix v 0 5 10 15 (aref m (aref sigma 8))  (aref m (aref sigma 9)))
        (mix v 1 6 11 12 (aref m (aref sigma 10)) (aref m (aref sigma 11)))
        (mix v 2 7 8  13 (aref m (aref sigma 12)) (aref m (aref sigma 13)))
        (mix v 3 4 9  14 (aref m (aref sigma 14)) (aref m (aref sigma 15)))))
    (coerce
     (map 'list #'logxor h (subseq v 0 8) (subseq v 8 16))
     '(u64-byte-array 8))))

(defun mix (array arr-a arr-b arr-c arr-d mx my)
  "Mixes the array with indexs `arr-a' `arr-b' `arr-c' `arr-d' and the
message value of `mx'`my', mutating the array at the four given indexs"
  (declare ((u64-byte-array 16) array))
  ;; why so mutative ☹
  (symbol-macrolet ((va (aref array arr-a))
                    (vb (aref array arr-b))
                    (vc (aref array arr-c))
                    (vd (aref array arr-d)))
    (setf va (mod (+ va vb mx) (expt 2 64)))
    (setf vd (rrot64 (logxor vd va) 32))

    (setf vc (mod (+ vc vd) (expt 2 64)))
    (setf vb (rrot64 (logxor vb vc) 24))

    (setf va (mod (+ va vb my) (expt 2 64)))
    (setf vd (rrot64 (logxor vd va) 16))

    (setf vc (mod (+ vc vd) (expt 2 64)))
    (setf vb (rrot64 (logxor vb vc) 63))))

(defun proper-endian (arr)
  "turns the output into the proper endian"
  (apply #'concatenate 'string
         (map 'list (lambda (x) (format nil "~{~(~2,'0X~)~}"
                                    (coerce
                                     (cl-intbytes:int->octets x 8)
                                     'list)))
              arr)))

;; longest function, think why this is correct, matches the java spec
;; I've found and played with in jshell
(defun rrot64 (x rot)
  "Magic formula to rotate and rot in 64"
  (mod (logior (ash x (- rot))
               (ash x (- 64 rot)))
   (expt 2 64)))


(-> byte-array-to-64-bit-word-array (byte-array) u64-byte-array)
(defun byte-array-to-64-bit-word-array (array)
  (let ((pad-array (pad-to-nearest-multiple array 8)))
    (coerce
     (mapcar (lambda (x)
               (cl-intbytes:octets->uint64 (subseq pad-array x (+ x 8))))
             (alexandria:iota (/ (length pad-array) 8)
                              :start 0 :step 8))
     'u64-byte-array)))

(defun lower-64bits (x)
  "Grabs the lower64 bits of the given number"
  (logand x #xFFFFFFFFFFFFFFFF))

(defun upper-64bits (x)
  "Grabs the upper64 bits of the given number"
  (logand x (ash #xFFFFFFFFFFFFFFFF 64)))


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
  (let ((length (length string)))
    (if (>= number length)
        string
        (make-string (- number length) :initial-element #\0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(uiop:define-package :misc.algorithms-test
  (:mix #:cl #:misc.algorithms  #:fiveam #:serapeum)
  (:export #:run-tests))

(in-package :misc.algorithms-test)
(def-suite algorithm.tests
  :description "Test the algorithms")

(in-suite algorithm.tests)

(test blake2b-512-expected
  (is (string= "a8add4bdddfd93e4877d2746e62817b116364a1fa7bc148d95090bc7333b3673f82401cf7aa2e4cb1ecd90296e3f14cb5413f8ed77be73045b13914cdcd6a918"
               (blake2 "The quick brown fox jumps over the lazy dog")))
  (is (string= "4b5a2e2e3efc44b602fde09133e3b197ba552a468e1c077552043a9db5f900d345b888fc71482a557cba3cbe1120083cc55c74276b9389de8f31c91bd5f069ac"
               (blake2 "1080p 1987 1991 1993 1998 2004 2006 2007 2014 2015 2016 320 3ds 4k 720 [as] adultswim album alcohol analog anne-clark arthouse bartending"))))


(defun run-tests ()
  (run! 'algorithm.tests))
