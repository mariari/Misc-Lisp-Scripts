;; Hash table version

(defparameter *nodes* (make-hash-table))
(defparameter *nodes-indirect* (make-hash-table))
(defparameter *nodes%%* (make-hash-table))

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


;;; Indirect way-----------------------------------------------------------------------------------------------

;; doesn't update by itself for some reason... most likely due to how passing via value instead of by reference works
;; !!Abandon this one!!!!!!!
(defun defnode%% (name hash &rest neighbors)
  (setf (gethash name hash)
        (let ((new-hash (make-hash-table)))
          (mapc (lambda (x) (if #1=(gethash x hash)
                           (setf (gethash x new-hash) #1#)
                           (setf (gethash x new-hash) (setf #1# t))))
                neighbors)
          new-hash)))


;; Just stores the symbol in the hash-table instead a reference to the top hash table
(defun defnode-bi-or-uni-ind (name hash bip &rest neighbors)
  (setf (gethash name hash)
        (let ((new-hash (make-hash-table)))
          (mapc (lambda (x)
                  (unless #1=(gethash x hash)
                    (if bip
                        (let ((new-node (make-hash-table))) ; bi direct
                          (setf (gethash x new-node) name) 
                          (setf #1# new-node))
                        (setf   #1# (make-hash-table)))) ;uni direct
                  (setf (gethash x new-hash) x))
                neighbors)
          new-hash)))

(defun defnode-ind (name hash &rest neighbors)
  (apply (curry defnode-bi-or-uni-ind name hash t) neighbors))

(defun defnode-uni-ind (name hash &rest neighbors)
  (apply (curry defnode-bi-or-uni-ind name hash nil) neighbors))



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
(defun defnode-bi-or-uni (name hash bip &rest neighbors)
  (setf (gethash name hash)
        (let ((new-hash (make-hash-table))
              (compiled nil))
          (lambda (&optional (comp compiled))
            (unless comp
              (mapc (lambda (x) (setf (gethash x new-hash) (gethash x hash))) neighbors) ; update the current node
              (setf compiled t))
            new-hash)))
  (mapc (lambda (x)
          (cond ((gethash x hash) (funcall (gethash x hash) nil)) ; update neighboring nodes to the current one if it exists
                (bip (defnode-bi-or-uni x hash bip name))         ; else make a bi-directional node
                (t (defnode-bi-or-uni x hash bip))))              ;            uni-directional node
        neighbors) 
  (gethash name hash))


(defun defnode (name hash &rest neighbors)
  (apply (curry defnode-bi-or-uni name hash t) neighbors))

(defun defnode-uni (name hash &rest neighbors)
  (apply (curry defnode-bi-or-uni name hash nil) neighbors))



(defun get-node (sym hash)
  (let ((val (gethash sym hash)))
    (when val
      (funcall val))))

(defun breadth-search-gen (start hash pred &key (key #'identity) limit)
  "Searches the graph from the START node breadth-first until it hits the PREDicate the user specified to the value
   of the KEY applied to the node.  The function can also check if a node is x nodes away if LIMIT is specified"
  (let ((seen (make-hash-table))
        (head) (tail)
        (lim (if limit (+ 2 limit) -1))) ; ends the loop properly if one wants to see
    ;; Nodes here are stored like this (b (b a)) to preserve path and are labeled node+
    (flet ((queue (node+)               ; the queue ensured breadth-first behavior
             (if (null head) 
                 (setf head (list node+)
                       tail head)
                 (setf (cdr tail) (list node+)
                       tail (cdr tail))))
           (dequeue ()
             (cond ((null head) nil)
                   ((eq head tail) (prog1 (car head)
                                     (setf head nil
                                           tail head)))
                   (t (pop head))))
           (see           (node)        (setf (gethash node seen) t))
           (seenp         (node)        (gethash node seen))
           (end-condition (node+)       (or (funcall pred (funcall key (car node+)))))
           (make-node+    (node node2+) (list node (cons node (cadr node2+)))))
      (declare (inline queue dequeue see seenp end-condition make-node+))
      (queue (list start (list start))) ; start the first node
      (see start)
      (loop :until (null head) :do
         (let ((curr (dequeue)))
           (when (end-condition curr)
             (return-from breadth-search-gen (list (car curr) (nreverse (cadr curr)))))
           (when (= lim 0) (return-from breadth-search-gen nil)) ; end prematurely if we can't hop any more nodes
           (decf lim)
           (loop :for sym being the hash-keys :of (get-node (car curr) hash) :do
              (unless (seenp sym)
                (queue (make-node+ sym curr))
                (see sym))))))))

;; QQ these don't work for breadth-search-gen
(defmacro queue (node+ head tail)
  `(if (null ,head) 
      (setf ,head (list ,node+)
            ,tail ,head)
      (setf (cdr ,tail) (list ,node+)
            ,tail (cdr ,tail))))

(defmacro dequeue (head tail)
  `(cond ((null ,head) nil)
        ((eq ,head ,tail) (prog1 (car ,head)
                          (setf ,head nil
                                ,tail ,head)))
        (t (pop ,head))))

;; Garbage at the moment since the stack frame will blow up if the sample size is big enough
(defun depth-search (start hash find  &key (key #'eq) (limit -1))
  (let ((seen)
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
      (rec (get-node start hash) (list start) limit seen)
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

;; (defun grab-node (term hash)
;;   (let ((val (gethash term hash)))
;;     (cond ((functionp val) (funcall val))       ;the closure version will show up as a function
;;           ((hash-table-p val) val)              ; the indirect one can either grab the hash table 
;;           ((symbolp val) (gethash val hash))))) ; or the symbol that leads to the hash

