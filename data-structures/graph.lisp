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
(defnode-ind 'A *nodes-indirect* 'B 'C 'D 'E)
(defnode-ind 'B *nodes-indirect* 'A 'D 'F)
(defnode-ind 'C *nodes-indirect*  'A 'F)
(defnode-ind 'D *nodes-indirect*  'A 'B)
(defnode-ind 'E *nodes-indirect*  'A)

(gethash 'b (gethash 'A *nodes-indirect*))

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


(defnode 'A *nodes* 'B 'C 'D 'E)
(defnode 'B *nodes* 'A 'D 'F)
(defnode 'C *nodes*  'A 'F)
(defnode 'D *nodes*  'A 'B)
(defnode 'E *nodes*  'A)
;; (defnode 'F *nodes*  'B)

(gethash 'B (funcall (gethash 'A *nodes*)))
(gethash 'B (funcall (gethash 'A *nodes*))) ; every node inside A is compiled

(defnode 'B *nodes* 'A 'D)

(funcall (gethash 'b (funcall (gethash 'a *nodes*))))


(apropos 'setf 'cl)
(mapcar #'documentation (apropos-list 'setf 'cl) '#1=(function . #1#))
