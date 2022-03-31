(defpackage :task

  (:use :clim :clim-lisp)
  (:export :app-main))

(in-package :task)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass stack-yaml-view (view)
  ((%yaml :initarg :stack-yaml :reader stack-yaml)))

(defclass show-view (view) ())

(defun wrap-yaml (yaml)
  (make-instance 'task :stack-yaml yaml))

(defclass task ()
  ((name     :initarg :name     :reader name)
   (children :initarg :children :reader children)
   (finished :initarg :finished :accessor finished)))

(defun make-task (name &rest children)
  (make-instance 'task :children children
                       :name name
                       :finished nil))

(defun mark-review (task)
  (setf (finished task) :review)
  task)

(defun mark-developing (task)
  (setf (finished task) :developing)
  task)

(defun mark-finished (task)
  (setf (finished task) t)
  task)

(defparameter *restore-world*
  (make-task 'restore-world))

(defparameter *llvm-haskell-context*
  (make-task 'llvm-haskell-context))

(defparameter *image*
  (make-task "IMAGE (easy or proper)"
             *restore-world*
             *llvm-haskell-context*))

(defparameter *easy-image*
  (make-task 'easy-image
             *image*))
(defparameter *proper-image*
  (make-task 'proper-image
             *image*))

(defparameter *string-ops*
  (make-task 'string-operations))
(defparameter *array-ops*
  (make-task 'array-operations))

(defparameter *closure-conversion*
  (make-task 'closure-conversion))

(defparameter *lambda-lifting*
  (make-task 'lambda-lifting))

(defparameter *currying*
  (make-task 'currying))

(defparameter *allocate-on-stack*
  (make-task 'allocation-on-the-stack))

(defparameter *recursive-adt*
  (make-task 'recursive-adt
             *list*))

(defparameter *list*
  (make-task 'LIST
             (make-task 'list-operations)))

(defparameter *borsch*
  (make-task 'borsch))

(defparameter *tasks*
  (make-task 'tasks
             (mark-finished
              (make-task 'closure
                         *closure-conversion*
                         *lambda-lifting*))
             (make-task 'escape-analysis
                        *closure-conversion*
                        *lambda-lifting*
                        *allocate-on-stack*)
             (make-task 'allocation-strategy
                        *allocate-on-stack*)
             (make-task 'gc
                        *proper-image*)
             (make-task 'berlin-pipeline
                        (make-task 'incremental-compilation
                                   *proper-image*
                                   *easy-image*)
                        (make-task 'declarations
                                   (mark-review
                                    (make-task 'sum-type-declaration
                                               *recursive-adt*
                                               *borsch*))
                                   (mark-review
                                    (make-task 'record-type-declaration
                                               *recursive-adt*
                                               *borsch*))
                                   (make-task 'function-declaration)))
             (mark-finished
              (make-task 'llvm-data-types
                         (mark-finished
                          (make-task 'Integers))
                         (make-task 'string
                                    *string-ops*)
                         (make-task 'array
                                    *array-ops*)))
             (make-task 'core-io-marking
                        *string-ops*
                        *array-ops*)
             (make-task 'tail-call-optimization)))

;;; the CLIM view class that corresponds to a list of members, one member
;;; per line of text in a CLIM application pane.
(defclass tasks-view (view) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-application-frame display-clim ()
  ((%top-task :initform *tasks* :accessor root))
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
  (let ((unwraped (root frame)))
    (format-graph-from-roots
     (list unwraped)
     (lambda (object stream)
       (present object (presentation-type-of object) :stream stream))
     #'children
     :stream pane
     :merge-duplicates t
     :maximize-generations t
     :graph-type :dag
     :center-nodes t
     ;; :orientation :vertical
     :generation-separation 20
     :within-generation-separation 20
     :arc-drawer #'my-arrow*
     :arc-drawing-options (list :line-thickness 1.4 :head-width 5))))

(defun views-example ()
  (run-frame-top-level (make-application-frame 'display-clim)))

(defun main-gui ()
  (views-example))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-display-clim-command (com-focus :name t) ((yaml 'task))
  (if yaml
      (setf (root *application-frame*) yaml)
      (format (frame-standard-input *application-frame*)
              "Please give a stack yaml"))
  (redisplay-frame-panes *application-frame* :force-p t))

(define-display-clim-command (com-reset :name t) ()
  (setf (root *application-frame*) *tasks*)
  (redisplay-frame-panes *application-frame* :force-p t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun present-task (pane task)
  (with-output-as-presentation (pane task 'task)
    (format pane "~A"
            (name task))))

(define-presentation-method present ((object task)
                                     (type   task)
                                     (stream extended-output-stream)
                                     (view   show-view)
                                     &key)
  (let ((color
          (case (finished object)
            ((t)          +pale-violet-red+)
            ((nil)        +alice-blue+)
            ((:review)    +pink+)
            ((:developing) +light-blue+))))
    (surrounding-output-with-border (stream :shape :rectangle
                                            :background color)
      (present-task stream object))))

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

(defun my-arrow* (stream from-node to-node x1 y1 x2 y2
                            &rest drawing-options
                            &key &allow-other-keys)
  (declare (ignore from-node to-node))
  (apply #'draw-arrow* stream x1 y1 x2 y2
         (append drawing-options (list :ink +black+))))

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

