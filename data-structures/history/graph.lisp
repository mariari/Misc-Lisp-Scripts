(defparameter *nodes-clos* (make-hash-table))
(defparameter *nodes-indirect* (make-hash-table))

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
  (apply #'defnode-bi-or-uni-ind name graph t neighbors))

(defun defnode-uni-ind (name graph &rest neighbors)
  (apply #'defnode-bi-or-uni-ind name graph nil neighbors))

;;; Testing Indirect
(defnode-ind 'A *nodes-indirect* 'B 'C 'D 'E)
(defnode-ind 'B *nodes-indirect* 'A 'D 'F)
(defnode-ind 'C *nodes-indirect*  'A 'F)
(defnode-ind 'D *nodes-indirect*  'A 'B)

;; (gethash 'b (gethash 'A *nodes-indirect*))

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
  (apply #'defnode-bi-or-uni-clos name graph t neighbors))

(defun defnode-uni-clos (name graph &rest neighbors)
  (apply #'defnode-bi-or-uni-clos name graph nil neighbors))


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

