(defparameter *nodes* '((living-room (you are in the living-room.
				      a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden.
				 there is a well in front of you.))
			(attic (you are in the attic.
				there is a giant welding torch in the corner.))))

(defparameter *edges* '((living-room (garden west door)
			 (attic upstairs ladder))
			(garden (living-room east door))
			(attic (living-room downstairs ladder))))
9
(defparameter *objects* '(whiskey bucket frog chain))

(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

(defparameter *location* 'living-room)

(defparameter *allowed-commands* '(look walk pickup inventory))

;; User inputs-----------------------------------------------
(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *object-locations*)))
;; kinda awkward since you need a quote before west --> 'west
(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *location* (car next))
	       (look))
	'(you cannot go that way.))))

(defun pickup (object)
  (cond ((member object
		 (objects-at *location* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object))
	(t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body  *object-locations*)))
;; Data used for user inputs---------------------------------
(defun describe-location (location nodes)
  ;; cadr removes (living-room (
  (cadr (assoc location nodes)))

;; Used in describe-paths 
(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

;; Higher level functions time
(defun describe-paths (location edges)
  ;; the apply basically reads the mapcar as multiple lists instead of 1 big one. so append can read it 
 (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))


;; Did it better than the book! 
(defun objects-at (loc obj-locs)
  ;; labels creates a local function 
  (labels ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p (mapcar #'car obj-locs))))


(defun describe-objects (loc obj-loc)
  (apply #'append (mapcar (lambda (obj)
			    `(you see a ,obj on the floor.))
			  (objects-at loc obj-loc))))


;;  REPL-----------------------------------------------------
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string
	      (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
	     (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
						  (prin1-to-string lst))
				     'list)
			     t
			     nil)
		 'string))
  (fresh-line))
;; Helper for print 
(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eq item #\space) (cons item (tweak-text rest caps lit)))
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
	    ((eq item #\") (tweak-text rest caps (not lit)))
	    (lit (cons item (tweak-text rest nil lit)))
	    ((or caps lit) (cons (char-upcase item) (tweak-text rest nil lit)))
	    (t (cons (char-downcase item) (tweak-text rest nil nil)))))))
;;  Testing--------------------------------------------------
(objects-at 'living-room *object-locations*)

(mapcar #'car *object-locations*)

(describe-objects 'living-room *object-locations*)

(find 'y '((5 x) (3 y) (7 z)) :key #'cadr)

(assoc *location* *edges*)
(print "foo")

(progn (print "this")
       (print "is")
       (print "a")
       (print "test"))

(progn (prin1 "this")
       (prin1 "is")
       (prin1 "a")
       (prin1 "test"))


(caadr '(walls (motor (3) 2) 2))
;; Code changed from how it was------------------------------

;;  removed

;; I made it not take objs, as it really isn't needed

;; (defun objects-at (loc objs obj-locs)
;;   (labels ((at-loc-p (obj)
;; 	     (eq (cadr (assoc obj obj-locs)) loc)))
;;     (remove-if-not #'at-loc-p objs)))

;; (objects-at 'living-room *objects* *object-locations*)

;; Added lambdas
;; (defun describe-objects (loc obj-loc)
;;   (labels ((describe-obj (obj)
;; 	     `(you see a ,obj on the floor.)))
;;     (apply #'append (mapcar #'describe-obj (objects-at loc obj-loc)))))
