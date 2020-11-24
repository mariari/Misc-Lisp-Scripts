
;;;; Dealing with CD's

;;; Makes the CD Data Structure 
(defun make-cd (title artist album rating ripped)
  (list :title title :album album :artist artist :rating rating :ripped ripped))

;;; the variable where we store the data
(defvar *db* nil)

(defun add-record(cd)
  "Adds a record to the database"
  (push cd *db*))

(defun dump-db()
  "Dump the Database in a readable format"
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun prompt-read(prompt)
  (format t "~a: " prompt)
  (force-output *query-io*)   ; to ensure that querry doesn't wait for a new line 
  (read-line *query-io*))


(defun prompt-for-cd()
  "Prompt the user to input data for the CD"
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (prompt-read "Album")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped[y/n]:")))

(defun add-cds ()
  "Adds multiple cds to the database untill the user says no"
  (loop (add-record (prompt-for-cd))
     (unless (y-or-n-p "Another?[y/n]: ") (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax (print *db* out))))


(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax (setf *db* (read in)))))



(add-record (make-cd "Roses" "Kathy mattea" 7 t))
(add-record (make-cd "Heaven" "I monster" "neveroddoreven" 10 t))
(add-record (make-cd "Aeroplane" "Neutral Milk Hotel" 7 t))
