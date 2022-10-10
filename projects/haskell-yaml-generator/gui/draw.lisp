(defpackage :clim-yaml
  (:use :clim :clim-lisp :cl-user)
  (:export main-gui))

(in-package :clim-yaml)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass stack-yaml-view (view)
  ((%yaml :initarg :stack-yaml :reader stack-yaml)))

(defclass stack-yaml-object-wraper ()
  ((%yaml :initarg :stack-yaml :reader stack-yaml)))

(defclass show-view (view) ())

(defun wrap-yaml (yaml)
  (make-instance 'stack-yaml-object-wraper :stack-yaml yaml))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-application-frame display-clim ()
  ((%top-stack :initform (wrap-yaml cl-user::*project*) :accessor root))
  (:panes
   (make-pane :application
              :width 600
              :height 800
              :display-function #'generate-graph
              :display-time t
              :default-view (make-instance 'show-view))
   (interactor :interactor :height 100 :width 100))
  (:layouts
   (default (vertically ()
              (9/10 make-pane)
              (1/10 interactor)))))

(defun generate-graph (frame pane)
  (let ((unwraped (stack-yaml (root frame))))
    (format-graph-from-roots
     (list unwraped)
     (lambda (object stream)
       (let ((wrap (wrap-yaml object)))
         (present wrap (presentation-type-of wrap) :stream stream)))
     ;; Filtered version of the function. Could call
     ;; #'cl-user::stack-yaml-packages if one wants madness of arrows!
     #'filter-unique-sub-packages
     :stream pane
     :merge-duplicates t
     :maximize-generations t
     :graph-type :dag
     :center-nodes t
     :orientation :vertical
     :generation-separation 20
     :within-generation-separation 20
     :arc-drawer #'draw-same-y-line*
     :arc-drawing-options (list :line-thickness 1.4 :head-width 5))))

(defun views-example ()
  (run-frame-top-level (make-application-frame 'display-clim)))

(defun main-gui ()
  (views-example))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filtering Children Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun filter-unique-sub-packages (package)
  "given a package, find all packages that entail all the dependent packages.
Thus if the given package relies on packages A and B, but package A
relies on B, then only give back A"
  (let* ((seen-set          (fset:convert 'fset:set (dependency-names package)))
         (children-packages (cl-user::stack-yaml-packages package))
         (unique-children   (reduce #'recursively-remove
                                    children-packages
                                    :initial-value seen-set)))
    (remove-if-not (lambda (package)
                     (fset:member? (cl-user::stack-yaml-name package) unique-children))
                   children-packages)))

(defun dependency-names (package)
  "gets the name of all the children packages"
  cl-user::(mapcar #'stack-yaml-name (stack-yaml-packages package)))

;; Could be done more generally by getting the list of children, then disjoint union
(defun recursively-remove (set package)
  "takes a set of names and a package and removes all generation of
children from the current set"
  (let ((reduced-set
          (reduce #'fset:less (dependency-names package) :initial-value set)))
    (reduce #'recursively-remove
            (cl-user::stack-yaml-packages package)
            :initial-value reduced-set)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-display-clim-command (com-focus :name t) ((yaml 'stack-yaml-object-wraper))
  (if yaml
      (setf (root *application-frame*) yaml)
      (format (frame-standard-input *application-frame*)
              "Please give a stack yaml"))
  (redisplay-frame-panes *application-frame* :force-p t))

(define-display-clim-command (com-reset :name t) ()
  (setf (root *application-frame*) (wrap-yaml cl-user::*project*))
  (redisplay-frame-panes *application-frame* :force-p t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun present-stack-yaml (pane stack-yaml-wraper)
  (with-output-as-presentation (pane stack-yaml-wraper 'stack-yaml-object-wraper)
    (let ((unwrapped (stack-yaml stack-yaml-wraper)))
      (format pane "~A"
              (cl-user::stack-yaml-name unwrapped)))))

(define-presentation-method present ((object stack-yaml-object-wraper)
                                     (type   stack-yaml-object-wraper)
                                     (stream extended-output-stream)
                                     (view   show-view)
                                     &key)
  (surrounding-output-with-border (stream :shape :rectangle :background +alice-blue+)
    (present-stack-yaml stream object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color Drawing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((inks (make-contrasting-inks 5)))
  (defun get-contrasting-ink (i)
    (elt inks (mod i 5))))

(defun draw-same-y-line* (stream from-node to-node x1 y1 x2 y2
                            &rest drawing-options
                            &key &allow-other-keys)
  (declare (ignore from-node to-node))
  (apply #'draw-arrow* stream x1 y1 x2 y2
         (append drawing-options (list :ink (get-contrasting-ink (floor y1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dragging functionality
;; Taken from the Demo code!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-graph-node (record)
  "Searches upward until a graph node parent of the supplied output record is found."
  (loop for current = record then (output-record-parent current)
        while current
        when (graph-node-output-record-p current)
          do (return current)))

(defun node-edges (node)
  (append (alexandria:hash-table-values (slot-value node 'climi::edges-from))
          (alexandria:hash-table-values (slot-value node 'climi::edges-to))))

(defun node-and-edges-region (node edges)
  (reduce #'region-union edges :key #'copy-rectangle
                               :initial-value (copy-rectangle node)))

(defun redisplay-edges (graph edges)
  (dolist (edge edges)
    (climi::layout-edge-1 graph (climi::from-node edge) (climi::to-node edge))))

;;; (AH) McCLIM bug of the day:
;;;
;;; (I haven't looked in detail at the spec or McCLIM to confirm my
;;; assumptions here, but as I understand things..)  CLIM regions are
;;; immutable. Output records ARE mutable. A McCLIM output record can
;;; be used as a rectangular region corresponding to its bounding
;;; rectangle.  But this bounding rectangle is not immutable! So,
;;; region operations such as region-union may build a rectangle-set
;;; capturing the mutable output-record object, violating the
;;; immutability of regions and causing widespread panic and
;;; confusion.
(defun copy-rectangle (region)
  (with-bounding-rectangle* (x0 y0 x1 y1) region
    ;; We use this rectangle to clear an area on the sheet which only
    ;; makes sense for integer coordinates.
    (make-rectangle* (floor x0) (floor y0) (ceiling x1) (ceiling y1))))

(define-display-clim-command (com-drag-node)
    ((record t) (offset-x real :default 0) (offset-y real :default 0))
  (let* ((stream *standard-output*)
         (node-record (find-graph-node record))
         (edge-records (node-edges node-record))
         (graph-record (output-record-parent node-record))
         (erase-region))
    (assert (typep graph-record 'graph-output-record))
    (drag-output-record
     stream node-record
     :feedback (lambda (record stream old-x old-y x y mode)
                 (declare (ignore old-x old-y))
                 (ecase mode
                   (:erase
                    ;; Capture current regions before modifying the
                    ;; output records.
                    (setf erase-region
                          (node-and-edges-region record edge-records))
                    ;; Remove contents (i.e. lines) of edge output
                    ;; records. This does not repaint anything. To
                    ;; account for that, we include ERASE-REGION in
                    ;; the :DRAW clause.
                    (map nil #'clear-output-record edge-records))
                   (:draw
                    ;; Reposition the node record (this does not
                    ;; automatically replay the record).
                    (setf (output-record-position record)
                          (values (- x offset-x) (- y offset-y)))
                    ;; Regenerate child records of the edge records
                    ;; for the changed node position (without drawing
                    ;; since we will draw everything at once as a
                    ;; final step).
                    (with-output-recording-options (stream :record t :draw nil)
                      (redisplay-edges graph-record edge-records))
                    ;; Repaint all affected areas. This also replays
                    ;; the modified node and edge output records.
                    (repaint-sheet
                     stream (region-union (or erase-region +nowhere+)
                                          (node-and-edges-region
                                           record edge-records))))))
     :finish-on-release t :multiple-window nil)))

(define-presentation-to-command-translator record-dragging-translator
    (t com-drag-node display-clim
     :tester ((object presentation)
              (find-graph-node presentation)))
    (object presentation x y)
  (multiple-value-bind (old-x old-y) (output-record-position presentation)
    (list presentation (- x old-x) (- y old-y))))
