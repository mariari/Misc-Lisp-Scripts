;;;; Dealing with CD's

;;; Global variables---------------------------------------------------------------------------------------------
(defparameter *my-db-title* (make-hash-table :test 'equalp))
(defvar *my-db-album* (make-hash-table :test 'equalp))
(defvar *my-db-artist* (make-hash-table :test 'equalp))
(defvar *my-db-ripped* (make-hash-table :test 'equalp))
(defvar *my-db-rating* (make-hash-table :test 'equalp))
(defvar *my-db* (list *my-db-title* *my-db-album* *my-db-artist* *my-db-ripped* *my-db-rating*))
(defvar *my-db-list* '(*my-db-title* *my-db-album* *my-db-artist* *my-db-ripped* *my-db-rating*))
(defvar *all-attributes* (list :title :album :artist :ripped :rating))
(defvar *user-attributes* nil)
;;;--------------------------------------------------------------------------------------------------------------

;;; Macros-------------------------------------------------------------------------------------------------------
(defmacro make-filename (name extension)
  "Converts a variable NAME to a string and appends EXTENSION on it *my-db-album lisp --> *MY-DB-ALUM*.lisp"
  `(concatenate  'string (string ',name) ,extension))

(defmacro global-var-to-filename% (name extension)
  "Converts a global variable NAME to a string and appends EXTENSION on it *my-db-album lisp --> MY-DB-ALUM.lisp"
  (let ((g (gensym)))                   ; Avoid unwanted variable capture
    `(let ((,g (string ',name)))
       (concatenate 'string (subseq ,g 1 (1- (length ,g))) ,extension))))

(defun global-var-to-filename (name extension)
  (let ((g (string name)))
    (concatenate 'string (subseq g 1 (1- (length g))) extension)))

;;; Data Structure-----------------------------------------------------------------------------------------------
(defun make-cd (title album artist rating ripped)
  (list :title title :album album :artist artist :rating rating :ripped ripped))
;;;--------------------------------------------------------------------------------------------------------------

;;; User Functions-----------------------------------------------------------------------------------------------
(defun my-add-record (cd &optional (*my-db* *my-db*) (*all-attributes* *all-attributes*))
  "Adds a record to all the different databases the user has registered"
  (mapc (lambda (db tags)
          (push cd (gethash (getf cd tags) db))) *my-db* *all-attributes*))

(defun my-dump-db ()
  "Dumps the database in a readable way"
  (maphash (lambda (keys values)
             (mapcar (lambda (value)         ; The values are like ((:title) (:title)) so map through it
                       (format t "~{~a:~10t~a~%~}~%" value)) values)) *my-db-album*))

(defun my-add-cds ()
  "Adds multiple CD's to the db"
  (loop (my-add-record (prompt-for-cd))
     (unless (y-or-n-p "Another?[y/n]: ") (return))))


(defun update-db (delimiter &key title album artist rating ripped)
  )

(defun my-save-db (&optional (filename *my-db-list*))
  "Saves all the config files in the database"
  (mapc (lambda (db)
          (with-open-file (out (global-var-to-filename db ".lisp")
                               :direction :output
                               :if-exists :supersede)
            (with-standard-io-syntax (write-hashtable (eval db) out)))) filename))

(defun my-load-db (&optional (filename *my-db-list*))
  "Saves all the config files in the database"
  (mapc (lambda (db)
          (with-open-file (in (global-var-to-filename db ".lisp"))
            (setf (symbol-value db) (read-hashtable in)))) filename))

;;; Auxiliary-helper-functions that make life easier-------------------------------------------------------------

(defun where (&key title artist rating (ripped nil ripped-p))
  (or (if title (gethash *my-db-title* title))
      (if artist)
      (if rating)
      (if ripped-p)))
;;;--------------------------------------------------------------------------------------------------------------
;;; Auxiliary-hash-table-functions-------------------------------------------------------------------------------
(defun write-hashtable (db &optional (stream *standard-output*))
  (maphash (lambda (key value) (print (list key value) stream)) db))

(defun read-hashtable (&optional (stream *standard-input*) (test #'equalp))
  (loop
     with table = (make-hash-table :test test) ; the hash table 
     with sentinel = (cons 1 1)                ; unique value for EOF
     for x = (read stream nil sentinel)        ; read value, sentinel if EOF
     until (eq sentinel x)                     ; until EOF, indicated by sentinel
     do (mapc (lambda (value)
                (push value (gethash (car x) table))) (cadr x))
     finally (return table)))
;;;--------------------------------------------------------------------------------------------------------------

;;; Testing -----------------------------------------------------------------------------------------------------
(my-add-record (make-cd "communist daughter" "Aeroplane" "Neutral Milk Hotelssss" 7 t))
(my-add-record (make-cd "heaven" "neveroddoreven" "I monster" 10 t))

;; (mapc #'make-filename '(1 2 3 bb4) '(".lisp" ".lisp" ".lisp" ".lisp"))
;; (mapcar (lambda (*test*) (global-var-to-filename% *test* ".lisp")) '(*123* *234* *3432* *42131*))

;; Loop way through has tables
;; (loop for key being each hash-key of *my-db-title*  
;;    using (hash-value value)
;;    do (print (list  value key)))
;;;--------------------------------------------------------------------------------------------------------------
