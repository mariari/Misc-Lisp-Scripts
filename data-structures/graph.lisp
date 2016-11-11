;; Hash table version

(defparameter *nodes* (make-hash-table))
(defparameter *nodes-indirect* (make-hash-table))
(defparameter *nodes%%* (make-hash-table))

;; Helper Macros-----------------------------------------------------------------------------------------------

(defmacro curry (fn . args)
  "Creates a partially applied function that takes 1 argument if it is a macro
   (a limitation of &rest closures in CL) and multiple if it is a function"
  (if (functionp (macro-function fn))
      `(currym ,fn ,@args)
      `(curryf #',fn ,@args)))

(defmacro currym (fn . args)
  "Creates a partially applied function that takes 1 argument"
  (let ((arg (gensym)))
    `(lambda (,arg) (,fn ,@args ,arg))))

(declaim (ftype (function (function &rest t) function) curryf)
         (inline curryf))
(defun curryf (fn &rest args)
  "Creates a partially applied function that takes many argument"
  (lambda (&rest args2) (apply fn (append args args2))))

(defmacro queue (node+ head tail)
  (let ((node (gensym)))
    `(let ((,node ,node+))
       (if (null ,head) 
           (setf ,head (list ,node)
                 ,tail ,head)
           (setf (cdr ,tail) (list ,node)
                 ,tail (cdr ,tail))))))

(defmacro dequeue (head tail)
  `(cond ((null ,head) nil)
        ((eq ,head ,tail) (prog1 (car ,head)
                          (psetf ,head nil
                                ,tail nil)))
        (t (pop ,head))))

;;; Indirect way-----------------------------------------------------------------------------------------------

;; doesn't update by itself for some reason... most likely due to how passing via value instead of by reference works
;; !!Abandon this one!!!!!!!
(defun defnode%% (name graph &rest neighbors)
  (setf (gethash name graph)
        (let ((new-hash (make-hash-table)))
          (mapc (lambda (x) (if #1=(gethash x graph)
                           (setf (gethash x new-hash) #1#)
                           (setf (gethash x new-hash) (setf #1# t))))
                neighbors)
          new-hash)))


;; Just stores the symbol in the hash-table instead a reference to the top graph table
(defun defnode-bi-or-uni-ind (name graph bip &rest neighbors)
  (setf (gethash name graph)
        (let ((new-hash (make-hash-table)))
          (mapc (lambda (x)
                  (unless #1=(gethash x graph)
                    (if bip
                        (let ((new-node (make-hash-table))) ; bi direct
                          (setf (gethash x new-node) name) 
                          (setf #1# new-node))
                        (setf   #1# (make-hash-table)))) ;uni direct
                  (setf (gethash x new-hash) x))
                neighbors)
          new-hash)))

(defun defnode-ind (name graph &rest neighbors)
  (apply (curry defnode-bi-or-uni-ind name graph t) neighbors))

(defun defnode-uni-ind (name graph &rest neighbors)
  (apply (curry defnode-bi-or-uni-ind name graph nil) neighbors))



;;; Testing Indirect
;; (time (defnode-ind 'A *nodes-indirect* 'B 'C 'D 'E))
(defnode-ind 'A *nodes-indirect* 'B 'C 'D 'E)
(defnode-ind 'B *nodes-indirect* 'A 'D 'F)
(defnode-ind 'C *nodes-indirect*  'A 'F)
(defnode-ind 'D *nodes-indirect*  'A 'B)
;; (time (defnode-ind 'D *nodes-indirect*  'A 'B))

(gethash 'b (gethash 'A *nodes-indirect*))

;; (time (gethash (gethash 'a (gethash (gethash 'b (gethash 'A *nodes-indirect*)) *nodes-indirect*)) *nodes-indirect*))

;;;  Closure style with proper nesting-------------------------------------------------------------------------
(defun defnode-bi-or-uni (name graph bip &rest neighbors)
  (setf (gethash name graph)
        (let ((new-hash (make-hash-table))
              (compiled nil))
          (lambda (&optional (comp compiled))
            (unless comp
              (mapc (lambda (x) (setf (gethash x new-hash) (gethash x graph))) neighbors) ; update the current node
              (setf compiled t))
            new-hash)))
  (mapc (lambda (x)
          (cond ((gethash x graph) (funcall (gethash x graph) nil)) ; update neighboring nodes to the current one if it exists
                (bip (defnode-bi-or-uni x graph bip name))          ; else make a bi-directional node
                (t (defnode-bi-or-uni x graph bip))))               ;            uni-directional node
        neighbors) 
  (gethash name graph))


(defun defnode (name graph &rest neighbors)
  (apply (curry defnode-bi-or-uni name graph t) neighbors))

(defun defnode-uni (name graph &rest neighbors)
  (apply (curry defnode-bi-or-uni name graph nil) neighbors))


(defun get-node (sym graph)
  (let ((val (gethash sym graph)))
    (when val
      (funcall val))))


(defun breadth-search-gen (start graph pred &key (key #'identity) limit)
  "Searches the graph from the START node breadth-first until it hits the PREDicate the user specified to the value
   of the KEY applied to the node.  The function can also check if a node is x nodes away if LIMIT is specified"
  (let ((seen (make-hash-table))
        (head) (tail)
        (lim (if limit (+ 2 limit) -1)))
    ;; Nodes here are stored like this (b (b a)) to preserve path and are labeled node+
    (flet ((see           (node)        (setf (gethash node seen) t))
           (seenp         (node)        (gethash node seen))
           (end-condition (node+)       (funcall pred (funcall key (car node+)))) ; node+ means the nodes are stored
           (make-node+    (node node2+) (list node (cons node (cadr node2+)))))   ; like (b (b a)) to preserve path
      (declare (inline see seenp end-condition make-node+))
      (queue (list start (list start)) head tail) ; start the first node
      (see start)
      (loop :until (null head) :do
         (let ((curr (dequeue head tail)))
           (when (end-condition curr)
             (return-from breadth-search-gen (list (car curr) (nreverse (cadr curr)))))
           (when (= lim 0) (return-from breadth-search-gen nil)) ; end prematurely if we hit our neighbor cap
           (decf lim)
           (loop :for sym :being the hash-keys :of (get-node (car curr) graph) :do
              (unless (seenp sym)
                (queue (make-node+ sym curr) head tail)
                (see sym))))))))

(defun breadth-search (start graph target &key (key #'identity) limit)
  (breadth-search-gen start graph (lambda (x) (eq target x)) :key key :limit limit))

;; Garbage at the moment since the stack frame will blow up if the sample size is big enough
(defun depth-search (start graph find  &key (key #'eq) (limit -1))
  (let ((seen (list start))
        (ans))
    (labels ((rec (node path lim seen)
               (if (= lim 0)
                   nil
                   (maphash (lambda (x y)
                              (when ans
                                (return-from rec))
                              (unless (member x seen)
                                (if (funcall key find x)
                                    (setf ans (list (car path) (reverse (cons find path))))
                                    (progn
                                      (rec (funcall y) (cons x path) (1- lim) (push x seen))))))
                            node))))
      (rec (get-node start graph) (list start) limit seen)
      (list seen ans))))

;; (maphash (lambda (x y) (princ x) (princ (funcall y))) (get-node 'a *nodes*))



(defnode 'A *nodes* 'B 'C 'D 'E)
;; (time (defnode 'A *nodes* 'B 'C 'D 'E))
(defnode 'B *nodes* 'A 'D 'F)
(defnode 'C *nodes*  'A 'F)
(defnode 'D *nodes*  'A 'B 'E 'G)
;; (time (defnode 'D *nodes*  'A 'B))


(gethash 'B (funcall (gethash 'A *nodes*)))
(gethash 'B (funcall (gethash 'A *nodes*))) ; every node inside A is compiled

;; (time (funcall (gethash 'a (funcall (gethash 'b (funcall (gethash 'a *nodes*)))))))

(defnode 'B *nodes* 'A 'D)

;; (defun grab-node (term graph)
;;   (let ((val (gethash term graph)))
;;     (cond ((functionp val) (funcall val))       ;the closure version will show up as a function
;;           ((hash-table-p val) val)              ; the indirect one can either grab the graph table 
;;           ((symbolp val) (gethash val graph))))) ; or the symbol that leads to the graph

