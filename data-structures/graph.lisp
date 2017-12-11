;; Hash table version
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:fset))
  (rename-package 'fset 'fset '(:fs)))


(defparameter *nodes-clos* (make-hash-table))
(defparameter *nodes-indirect* (make-hash-table))
(defparameter *nodes* (make-hash-table))

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
  `(cond ((null ,head)
          nil)
         ((eq ,head ,tail)
          (prog1 (car ,head)
                 (psetf ,head nil
                        ,tail nil)))
         (t (pop ,head))))

;;; Pointers from lol-----------------------------------------------------------------------------------
(defmacro pointer-& (obj)
  "Simulates a pass by reference"
  (let ((g!set (gensym))
        (g!temp (gensym)))
    `(lambda (&optional (,g!set ',g!temp))
       (if (eq ,g!set ',g!temp)
           ,obj
           (setf ,obj ,g!set)))))

(defun pointer-* (addr)
  "Emulates a pass by value"
  (funcall addr))

(defsetf pointer-* (addr) (val)
  `(funcall ,addr ,val))

(defsetf pointer-& (addr) (val)
  `(setf (pointer-* ,addr) ,val))

;;; Indirect way-----------------------------------------------------------------------------------------------

;; doesn't update by itself for some reason... most likely due to how passing via value instead of by reference works
;; !!Abandon this one!!
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
                  (unless #1=(gethash x graph)              ; if the node we are setting does not exist
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

;;;  Closure style with proper nesting Worse than the ptr way--------------------------------------------------
(defun defnode-bi-or-uni-clos (name graph bip &rest neighbors)
  (setf (gethash name graph)
        (let ((new-hash (make-hash-table))
              (compiled nil))
          (lambda (&optional (comp compiled))
            (unless comp
              (mapc (lambda (x) (setf (gethash x new-hash) (gethash x graph))) neighbors) ; update the current node
              (setf compiled t))
            new-hash)))
  (mapc (lambda (x)
          (cond ((gethash x graph) (funcall (gethash x graph) nil))           ; update neighboring nodes to the current one if it exists
                (bip               (defnode-bi-or-uni-clos x graph bip name)) ; else make a bi-directional node
                (t                 (defnode-bi-or-uni-clos x graph bip))))    ; uni-directional node
        neighbors)
  (gethash name graph))


(defun defnode-clos (name graph &rest neighbors)
  (apply (curry defnode-bi-or-uni-clos name graph t) neighbors))

(defun defnode-uni-clos (name graph &rest neighbors)
  (apply (curry defnode-bi-or-uni-clos name graph nil) neighbors))


;; (maphash (lambda (x y) (princ x) (princ (funcall y))) (get-node 'a *nodes-clos*))

(defnode-clos 'A *nodes-clos* 'B 'C 'D 'E)
;; (time (defnode-clos 'A *nodes-clos* 'B 'C 'D 'E))
(defnode-clos 'B *nodes-clos* 'A 'D 'F)
(defnode-clos 'C *nodes-clos*  'A 'F)
(defnode-clos 'D *nodes-clos*  'A 'B 'E 'G)
;; (time (defnode-clos 'D *nodes-clos*  'A 'B))


(gethash 'B (funcall (gethash 'A *nodes-clos*)))
(gethash 'B (funcall (gethash 'A *nodes-clos*))) ; every node inside A is compiled

;; (time (funcall (gethash 'a (funcall (gethash 'b (funcall (gethash 'a *nodes-clos*)))))))

(defnode-clos 'B *nodes-clos* 'A 'D)


;;; Pointer style from LOL (uses Closures)---------------------------------------------------------------------
(defun defnode%%% (name graph &rest neighbors)
  (setf (gethash name graph)
        (let ((new-hash (make-hash-table)))
          (mapc (lambda (x) (setf (gethash x new-hash) (pointer-& (gethash x graph)))) neighbors)
          new-hash)))

(defun defnode-bi-or-uni-ptr (name graph bip &rest neighbors)
  (setf (gethash name graph)
        (let ((new-hash (make-hash-table)))
          (mapc (lambda (x)
                  (unless #1=(gethash x graph)                    ; if the node we are setting does not exist
                          (if bip
                              (let ((new-node (make-hash-table))) ; bi direct
                                (setf (gethash x new-node) (pointer-& (gethash name graph)))
                                (setf #1# new-node))
                              (setf #1# (make-hash-table))))
                  (setf (gethash x new-hash) (pointer-& (gethash x graph)))) ; set the newhash to it
                neighbors)
          new-hash)))


(defun defnode (name graph &rest neighbors)
  (apply (curry defnode-bi-or-uni-ptr name graph t) neighbors))

(defun defnode-uni (name graph &rest neighbors)
  (apply (curry defnode-bi-or-uni-ptr name graph nil) neighbors))


(defun get-node (sym graph)
  (let ((val (gethash sym graph)))
    (if (functionp val)
        (funcall val)
        val)))


(defun breadth-search-gen (start graph pred &key (key #'identity) limit)
  "Searches the graph from the START node breadth-first until it hits the PREDicate the user specified to the value
   of the KEY applied to the node.  The function can also check if a node is x nodes away if LIMIT is specified"
  (let ((seen (make-hash-table))
        (head)
        (tail)
        (lim (if limit (+ 2 limit) -1)))
    ;; Nodes here are stored like this (b (b a)) to preserve path and are labeled node+
    (flet ((see           (node)        (setf (gethash node seen) t))
           (seenp         (node)        (gethash node seen))
           (end-condition (node+)       (funcall pred (funcall key (car node+)))) ; node+ means the nodes are stored
           (make-node+    (node node2+) (list node (cons node (cadr node2+))))) ; like (b (b a)) to preserve path
      (declare (inline see seenp end-condition make-node+))
      (queue (list start (list start)) head tail) ; start the first node
      (see start)
      (loop :until (null head) :do
         (let ((curr (dequeue head tail)))
           (when (end-condition curr)
             (return-from breadth-search-gen (list (car curr)
                                                   (nreverse (cadr curr)))))
           (when (= lim 0)
             (return-from breadth-search-gen nil)) ; end prematurely if we hit our neighbor cap
           (decf lim)
           (loop :for sym :being :the hash-keys :of (get-node (car curr) graph) :do
              (unless (seenp sym)
                (queue (make-node+ sym curr) head tail)
                (see sym))))))))

(defun breadth-search (start graph target &key (key #'identity) limit)
  (breadth-search-gen start graph (lambda (x) (eq target x)) :key key :limit limit))

;; Garbage at the moment since the stack frame will blow up if the sample size is big enough
;; oddly enough faster than the fset version but not the hash table version of depth-search%
;; something in this code makes it fast and does less bytes consed
(defun depth-search (start graph find &key (key #'eq) (limit -1))
  (let ((seen (make-hash-table)))
    (setf (gethash start seen) t)
    (labels ((rec (node path lim)
               (if (= lim 0)
                   nil
                   (maphash (lambda (x y)
                              (unless (gethash x seen)
                                (setf (gethash x seen) t)
                                (if (funcall key find x)
                                    (return-from depth-search (reverse (cons find path)))
                                    (rec (funcall y) (cons x path) (1- lim)))))
                            node))))
      (rec (get-node start graph) (list start) limit))))

(defun hash-keys (hash)
  (loop for key being the hash-keys of hash collect key))

(defun hash-values (hash)
  (loop for value being the hash-values of hash collect value))


;; lazy edition
;; These versions are almost as fast as the version that could theoretically blow up the stack
;; they are
(defun depth-searchl% (start graph find &key (key #'eq) (limit -1))
  (labels ((rec (node node-list lim seen path)
             (flet ((recurse (nodes-list)
                      (let ((next (scar nodes-list)))
                        (rec (car next) (scdr nodes-list) (caddr next) (fs:with seen node) (scar (cdr next)))))
                    (prune (lis)   (sfilter (lambda (x) (not (fs:contains? seen x))) lis))
                    (add   (x) (list x (scons node path) (1- lim))))

               (cond ((null node-list)          '())
                     ((funcall key find node)    (sreverse (scons find path)))
                     ((or (null node) (= lim 0)) (recurse node-list))
                     (t                          (recurse (sappend (smap #'add (prune (hash-keys
                                                                                       (get-node node graph))))
                                                                   node-list)))))))
    (make-strict (rec start '(()) limit (fs:empty-set) '()))))

;; This version for whatever reason is a LOT faster when the
;; the answer can't be found... like 100x faster for (depth-searchl 1 *nodes* 122200)
;; I'm guessing the hash table does better
;; it's probably because the hash table gets its answers right away!
;; but I don't see how that effects what nodes are evaluated when, as it should get called anyways
;; (ie its a strict operation)
(defun depth-searchl (start graph find &key (key #'eq) (limit -1))
  (let ((seen (make-hash-table)))
    (labels ((rec (node node-list lim path)
               (flet ((recurse (nodes-list)
                        (let ((next (scar nodes-list)))
                          (setf (gethash node seen) t)
                          (rec (car next) (scdr nodes-list) (caddr next) (scar (cdr next)))))
                      (prune (lis)   (sfilter (lambda (x) (not (gethash x seen))) lis))
                      (add   (x) (list x (scons node path) (1- lim))))

                 (cond ((null node-list)          '())
                       ((funcall key find node)    (sreverse (scons find path)))
                       ((or (null node) (= lim 0)) (recurse node-list))
                       (t                          (recurse (sappend (smap #'add (prune (hash-keys
                                                                                         (get-node node graph))))
                                                                     node-list)))))))
      (make-strict (rec start '(()) limit '())))))

;; These versions are slower than the old version, take more memory, but won't blow up stack frames
;; this is more inline to my current style of problem solving...
;; node is stored as (val (list) lim)
;; I use a fset data structure to get better time complexity
;; but fset seems to be incredibly slow and thus should be written with an impure hash table as below
(defun depth-search% (start graph find &key (key #'eq) (limit -1))
  (labels ((rec (node node-list lim seen path)
             (flet ((recurse (nodes-list)
                      (let ((next (car nodes-list)))
                        (rec (car next) (cdr nodes-list) (caddr next) (fs:with seen node) (car (cdr next)))))
                    (prune (lis)   (remove-if (lambda (x) (fs:contains? seen x)) lis))
                    (add   (acc x) (cons (list x (cons node path) (1- lim)) acc)))

               (cond ((null node-list)          '())
                     ((funcall key find node)    (reverse (cons find path)))
                     ((or (null node) (= lim 0)) (recurse node-list))
                     (t                          (recurse (reduce #'add (prune (hash-keys (get-node node graph)))
                                                                  :initial-value node-list)))))))
    (rec start '(()) limit (fs:empty-set) '())))

;; This is a faster version of the code above
;; Furthermore the time complexity on gethash is O(1) so it's just superior in general
;; Except if you care about purity (like I do)
(defun depth-search%% (start graph find &key (key #'eq) (limit -1))
  (let ((seen (make-hash-table)))
    (labels ((rec (node node-list lim path)
               (flet ((recurse (nodes-list)
                        (let ((next (car nodes-list)))
                          (setf (gethash node seen) t)
                          (rec (car next) (cdr nodes-list) (caddr next) (cadr next))))
                      (add (acc x)
                        (cons (list x (cons node path) (1- lim)) acc)))
                 (cond ((null node-list)          '())
                       ((funcall key find node)    (reverse (cons find path)))
                       ((or (null node) (= lim 0)) (recurse node-list))
                       (t                          (recurse (reduce #'add
                                                                    (remove-if (lambda (x) (gethash x seen))
                                                                               (hash-keys (get-node node graph)))
                                                                    :initial-value node-list)))))))
      (rec start '(()) limit '()))))

;; a linked list version to test speed against
(defun depth-search%%% (start graph find &key (key #'eq) (limit -1))
  (labels ((rec (node node-list lim seen path)
             (flet ((recurse (nodes-list)
                      (let ((next (car nodes-list)))
                        (rec (car next) (cdr nodes-list) (caddr next) (cons node seen) (cadr next))))
                    (add (acc x)
                      (cons (list x (cons node path) (1- lim)) acc)))
               (cond ((null node-list)          '())
                     ((funcall key find node)    (reverse (cons find path)))
                     ((or (null node) (= lim 0)) (recurse node-list))
                     (t                          (recurse (reduce #'add
                                                                  (remove-if (lambda (x) (member x seen))
                                                                             (hash-keys (get-node node graph)))
                                                                  :initial-value node-list)))))))
    (rec start '(()) limit '() '())))

(defnode 'A *nodes* 'B 'C 'D 'E)
;; (time (defnode 'A *nodes* 'B 'C 'D 'E))
(defnode 'B *nodes* 'A 'D 'F)
(defnode 'C *nodes*  'A 'F)
(defnode 'D *nodes*  'A 'B 'E 'G)
(defnode 'D *nodes*  'A 'B 'E 'G)
(defnode 'D *nodes*  'A 'B 'E 'G)
;; (time (defnode 'D *nodes*  'A 'B))
(loop for i from 0 to 1000
      :do (apply #'defnode i *nodes* (loop for i from 0 to (random 100) collect (random 1000))))

(defnode 'B *nodes* 'A 'D)

;; (defun grab-node (term graph)
;;   (let ((val (gethash term graph)))
;;     (cond ((functionp val) (funcall val))       ;the closure version will show up as a function
;;           ((hash-table-p val) val)              ; the indirect one can either grab the graph table
;;           ((symbolp val) (gethash val graph))))) ; or the symbol that leads to the graph



;; foldr is much slower than (sappend (smap ...)) for some reason
(defun depth-searchl%% (start graph find &key (key #'eq) (limit -1))
  (labels ((rec (node node-list lim seen path)
             (flet ((recurse (nodes-list)
                      (let ((next (scar nodes-list)))
                        (rec (car next) (scdr nodes-list) (caddr next) (fs:with seen node) (scar (cdr next)))))
                    (prune (lis)   (remove-if (lambda (x) (fs:contains? seen x)) lis))
                    (add   (x acc) (scons (list x (scons node path) (1- lim)) acc)))

               (cond ((null node-list)          '())
                     ((funcall key find node)    (sreverse (scons find path)))
                     ((or (null node) (= lim 0)) (recurse node-list))
                     (t                          (recurse (sfoldr #'add node-list
                                                                  (prune (hash-keys (get-node node graph))))))))))
    (make-strict (rec start '(()) limit (fs:empty-set) '()))))


;; (time (defparameter *result* (make-strict (depth-searchl% 1 *nodes* 21))))
;; (time (defparameter *result* (depth-search% 1 *nodes* 21)))