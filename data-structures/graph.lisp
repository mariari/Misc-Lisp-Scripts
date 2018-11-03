(defpackage #:graph
  (:documentation "provides an imperative graph data structure")
  (:use :cl
        :reference
        :lcons)
  (:export :defnode
           :defnode-uni
           :get-node
           :remove-outgoing-edges
           :breadth-search
           :depth-search
           :depth-searchl))

(in-package :graph)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(:fset))
  (rename-package 'fset 'fset '(:fs)))

;; Helper Macros-----------------------------------------------------------------------------------------------

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

;;; Ref style from OCAML --------------------------------------------------------------------------------------

(defun defnode-bi-or-uni-ptr (name graph creation neighbors)
  (let ((node (if (gethash name graph)
                  (gethash name graph)
                  (setf (gethash name graph) (ref (make-hash-table))))))
    (mapc (lambda (x)
            (unless (gethash x graph)                      ; creates the node with the given function
              (funcall creation x))                        ; if it's not already in the hash-table
            (setf (gethash x (! node)) (gethash x graph))) ; already a reference
          neighbors)))

(defun defnode (name graph &rest neighbors)
  "Creates a graph with bidirectional edges"
  (defnode-bi-or-uni-ptr
      name graph
    (lambda (neighbor)                                ; creation function if it's not in global node
      (let ((neighbor-contents (make-hash-table))     ; this will be the contents of the neighbor
            (node              (gethash name graph))) ; node is a reference
        (setf (gethash name neighbor-contents) node)  ; make the neighbor have the node
        (setf (gethash neighbor graph) (ref neighbor-contents)))) ; put a ref to this in the global
    neighbors))

(defun defnode-uni (name graph &rest neighbors)
  (defnode-bi-or-uni-ptr
      name graph
    (lambda (neighbor) ; creation function if it's not in the global node
      (setf (gethash neighbor graph) (ref (make-hash-table))))
    neighbors))

(defun get-node (sym graph)
  (let ((val (gethash sym graph)))
    (when val (! val))))

(defun remove-outgoing-edges (sym graph)
  (let ((val (gethash sym graph)))
    (:= val (make-hash-table))))

;; Algorithms on the graph------------------------------------------------------------------------------------


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
                                    (rec (! y) (cons x path) (1- lim)))))
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
             (flet ((prune   (lis)        (sfilter (lambda (x) (not (fs:contains? seen x))) lis))
                    (add     (x)          (list x (scons node path) (1- lim)))
                    (recurse (nodes-list) (let ((next (scar nodes-list)))
                                            (rec (car next) (scdr nodes-list) (caddr next) (fs:with seen node)
                                                 (scar (cdr next))))))
               (cond ((null node-list)          '())
                     ((funcall key find node)    (sreverse (scons find path)))
                     ((or (null node) (= lim 0)) (recurse node-list))
                     (t                          (recurse (sappend (smap #'add (prune (hash-keys (get-node node graph))))
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
               (flet ((prune   (lis)        (sfilter (lambda (x) (not (gethash x seen))) lis))
                      (add     (x)          (list x (scons node path) (1- lim)))
                      (recurse (nodes-list) (let ((next (scar nodes-list)))
                                              (setf (gethash node seen) t)
                                              (rec (car next) (scdr nodes-list) (caddr next)
                                                   (scar (cdr next))))))
                 (cond ((null node-list)          '())
                       ((funcall key find node)    (sreverse (scons find path)))
                       ((or (null node) (= lim 0)) (recurse node-list))
                       (t                          (recurse (sappend (smap #'add (prune (hash-keys (get-node node graph))))
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
