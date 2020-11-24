;; 2.2 A straightfoward solution-------------------------------------------------------
(defun sentence    () (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (article) (Noun)))
(defun PP () (append (Prep) (noun-phrase)))
(defun Adj () (one-of '(big little blue green adiabatic)))
(defun Prep () (one-of  '(to in by with on)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article     () (one-of '(the a)))
(defun Noun        () (one-of '(man ball woman table)))
(defun Verb        () (one-of '(hit took saw liked)))


(defun one-of (set)
  "Pick one element of a set, and make a list of it"
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random"
  (nth (random (length choices)) choices))

(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
      nil))

;; 2.3 A Rule-Based Solution-----------------------------------------------------------
(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))

  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate. Initially , this is
*simple-grammar*, but we can switch to other grammars.")

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (car rule))

(defun rule-rhs (rule)
  "The righgt-hand side of a rule."
  (cddr rule))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun mappend (fn list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn list)))


(defun generate (phrase)
  "Generates a random sentence or phrase"
  (cond ((listp phrase)
         (mappend #'generate phrase))
        ((rewrites phrase)
         (generate (random-elt (rewrites phrase))))
        (t (list phrase))))

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP*) (Name) (Pronoun))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabatic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that)))



(setf *grammar* *bigger-grammar*)


;; Same thing but we want this in a list form

(defun generate-tree (phrase)
  "Generates a random sentence or phrase"
  (cond ((listp phrase)
         (mapcar #'generate-tree phrase))
        ((rewrites phrase)
         (cons phrase  (generate-tree (random-elt (rewrites phrase)))))
        (t (list phrase))))

(defun generate-all (phrase)
  "Generate a list of all possible expansions of this phrase."
  (cond ((null phrase) (list nil))
        ((listp phrase)
         (combine-all (generate-all (car phrase))
                      (generate-all (cdr phrase))))
        ((rewrites phrase)
         (mappend #'generate-all (rewrites phrase)))
        (t (list (list phrase)))))

(defun combine-all (xlist ylist)
  "Return a list of lists formed by appending a y to an x.
   E.G. (combine-all '((a) (b)) '((1) (2))) -> ((A 1) (B 1) (A 2) (B 2)"
  (mappend (lambda (y)
             (mapcar (lambda (x) (append x y)) xlist))
           ylist))
;; Need to use simple grammar or the code will break due to recursive 
(defparameter *grammar* *simple-grammar*)

(length (generate-all 'sentence))



(defun cross-product (fn xlist ylist)
  "Returns a list of all (fn x y) values."
  (mappend (lambda (x) (mapcar (lambda (y) (funcall fn y x))
                          xlist))
           ylist))
