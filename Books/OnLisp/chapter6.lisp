;; Nodes the normal way
(defstruct node2 contents yes no)

(defvar *nodes2* (make-hash-table))

(defun defnode2 (name conts &optional yes no)
  (setf (gethash name *nodes2*)
        (make-node2 :contents conts
                    :yes yes
                    :no no)))

(defun run-node (name)
  (let ((n (gethash name *nodes2*)))
    (cond ((node2-yes n)
           (format t "~A~%>> " (node2-contents n))
           (case (read)
             (yes (run-node (node2-yes n)))
             (t   (run-node (node2-no n)))))
          (t (node2-contents n)))))

(defnode2 'people "is this person a man" 'male 'female)

(defnode2 'male "Is he living?" 'liveman 'deadman)

(defnode2 'deadman "Was he American" 'us 'them)

(defnode2 'us "Is he on a coin?" 'coin 'cidence)

(defnode2 'coin "Is the coin a penny?" 'penny 'coins)

(defnode2 'penny 'lincoln)
;; (run-node 'us) --> Is he on a coin?
;; >> yes
;; LINCOLN

;; Closure way-------------------------------------------
(defvar *nodes1* (make-hash-table))

(defun defnode1 (name conts &optional yes no)
  (setf (gethash name *nodes1*)
        (if yes
            (lambda ()
              (format t "~A~%" conts)
              (case (read)
                (yes (funcall (gethash yes *nodes1*)))
                (t   (funcall (gethash no *nodes1*)))))
            (lambda () conts))))


(defnode1 'people "is this person a man" 'male 'female)

(defnode1 'male "Is he living?" 'liveman 'deadman)

(defnode1 'deadman "Was he American" 'us 'them)

(defnode1 'us "Is he on a coin?" 'coin 'cidence)

(defnode1 'coin "Is the coin a penny?" 'penny 'coins)

(defnode1 'penny 'lincoln)

(defnode1 'people "are they a man" 'male 'female)
(defnode1 'male 'lincoln)
(defnode1 'female 'lincolnbutgirl)



;; (funcall (defnode1 'coin "Is the coin a penny?" 'penny 'coins))
;; (funcall (gethash 'people *nodes1*))
;; 6.2 Compiling Networks--------------------------------------------

;; We can write this in a simpler way,
(defvar *nodes* nil)

(defun defnode (&rest args)
  (push args *nodes*)
  args)

;; Compiles the program and does the O(N) behavior right before compile, and when you use it it's O(1)

(defun compile-net (root)
  (let ((node (assoc root *nodes*)))
    (unless (null node)
      (let ((conts (second node))
            (yes (third node))
            (no (fourth node)))
        (if yes             ; If there are any more trees to traverse 
            (let ((yes-fn (compile-net yes)) ; Recursively goes down the list 
                  (no-fn (compile-net no)))
              (lambda ()
                (format t "~A~%>> " conts)
                (funcall (if (eq (read) 'yes)
                             yes-fn
                             no-fn))))
            (lambda () conts))))))          ; Else the contents are printed

;; Same as compile-net but removed the let.. which makes it O(N) at run time since the let was before the lambda 
(defun compile-net2 (root)
  (let ((node (assoc root *nodes*)))
    (if (null node)
        nil
        (let ((conts (second node))
              (yes (third node))
              (no  (fourth node)))
          (if yes
              (lambda ()
                (format t "~A~%>> " conts)
                (funcall (if (eq (read) 'yes)
                             (compile-net2 yes)
                             (compile-net2 no))))
              (lambda () conts))))))


(defparameter n (compile-net 'people))
;; (funcall n)
;; From this point foward, *nodes* is disposable and can be thrown away 



;; Helping debug

(defstruct node contents yes no)

(defparameter *nodes* (make-hash-table))

(defun defnode (name conts &optional yes no)
   (setf (gethash name *nodes*)
         (if yes
             (lambda ()
               (format t "~A~%>> " conts)
               (case (read)
                 (yes (funcall (gethash yes *nodes*)))
                 (t
                  (funcall (gethash no *nodes*)))))
             (lambda () conts))))

(defnode 'people "is they a mman" 'male 'female)
(defnode 'male 'lincoln)
(defnode 'female 'lincolnbutgirl)
