(defparameter *test-nodes* '(((LIVING-ROOM
			       (YOU ARE IN THE LIVING-ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH.))
			      (GARDEN
			       (YOU ARE IN A BEAUTIFUL GARDEN. THERE IS A WELL IN FRONT OF YOU.))
			      (ATTIC
			       (YOU ARE IN THE ATTIC. THERE IS A GIANT WELDING TORCH IN THE CORNER.)))))
(defparameter *test-nodes-smaller* '((LIVING-ROOM
				      (YOU ARE IN THE LIVING-ROOM. A WIZARD IS SNORING LOUDLY ON THE COUCH.))
				     (GARDEN
				      (YOU ARE IN A BEAUTIFUL GARDEN. THERE IS A WELL IN FRONT OF YOU.))
				     (ATTIC
				      (YOU ARE IN THE ATTIC. THERE IS A GIANT WELDING TORCH IN THE CORNER.)))) 

;; All these statements are = to each other and the result = --------------------------------
(apply #'append (apply #'print *test-nodes*))

(apply (lambda (node) (apply #'append node)) *test-nodes*)

(apply #'(apply #'append) *test-nodes*)

(defparameter *x* (apply #'print *test-nodes*))

(apply #'append *x*)
;; ------------------------------------------------------------------------------------------


;; apply basically makes it test-nodes-smaller for whatever shall be applied to it  
(apply #'+ (apply #'append (apply #'append '(((1) (2) (3) (3 2 3) (1 2 3 4 )) ((1) (2) (3))))))

(apply #'append '((1 2 3) (a b c)))

(append '(1 2 3) '(a b c))

;; to see the difference between apply and mapcar
(defun self-and-double (x) (list x (+ x x)))

;; Apply simply just inserts itself into the first parathesis spot '(3) --> (self-and-double 3)
(apply #'self-and-double '(3))

;; (3 6)

(mapcar #'self-and-double '(1 10 300))
;; ((1 2) (10 20) (300 600))


