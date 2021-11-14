;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (ql:quickload :mcclim)
;;   (ql:quickload :clim))

(defpackage :sheet-example
  (:use :clim :clim-lisp)
  (:export :app-main))

(in-package :sheet-example)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defclass person ()
;;   ((%last-name :initarg :last-name :accessor person-last-name)
;;    (%first-name :initarg :first-name :accessor person-first-name)
;;    (%address :initarg :address :accessor person-address)
;;    (%membership-number :initarg :number :reader person-number)))

(defstruct person
  last-name
  first-name
  address
  number)

(defclass person-wraper ()
  ((%person :initarg :person :reader person)))

(defclass members-view (view) ())

(defparameter *members-view* (make-instance 'members-view))

(defun create-person (last-name first-name address membership-number)
  (make-person
   :last-name last-name
   :first-name first-name
   :address address
   :number membership-number)
  ;; (make-instance 'person
  ;;                :last-name last-name
  ;;                :first-name first-name
  ;;                :address address
  ;;                :number membership-number)
  )

(defparameter *members*
  (list (create-person "Doe" "Jane" "123, Glencoe Terrace" 12345)
        (create-person "Dupont" "Jean" "111, Rue de la Republique" 54321)
        (create-person "Smith" "Eliza" "22, Trafalgar Square" 121212)
        (create-person "Nilsson" "Sven" "Uppsalagatan 33" 98765)))

(defclass person-view (view)
  ((%person :initarg :person :reader person)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric display-pane-with-view (frame pane view))

(defmethod display-pane-with-view (frame pane (view person-view))
  (let ((person (person view)))
    (format pane "Last name: ~a~%First Name: ~a~%Address: ~a~%Membership Number: ~a~%"
            (person-last-name person)
            (person-first-name person)
            (person-address person)
            (person-number person))))

(defmethod display-pane-with-view (frame pane (view members-view))
  (mapc (lambda (member)
          (let ((wrap (make-instance 'person-wraper :person member)))
            (with-output-as-presentation (pane wrap 'person-wraper)
              (format pane "~a, ~a, ~a, ~a~%"
                      (person-number member)
                      (person-last-name member)
                      (person-first-name member)
                      (person-address member)))))
        (members frame)))


(defun display-main-pane (frame pane)
  (display-pane-with-view frame pane (stream-default-view pane)))

(define-application-frame views ()
  ;; Storage
  ((%members :initform *members* :accessor members)
   (%root :initform (make-instance 'trash) :accessor root))
  ;; Panes
  (:panes
   (main-pane :application
              :height 500
              :width 500
              :display-function #'display-main-pane
              ;; notice the initialization of the default view of
              ;; the application pane.
              :default-view *members-view*)
   (files :application
          :display-function (lambda (frame pane)
                              (let ((root (root frame)))
                                (present-trash root pane)))
          :default-view (make-instance 'file-manager-view))
   (interactor :interactor :height 100 :width 500))
  (:layouts
   (default
    (horizontally ()
      files
      (vertically ()
        main-pane
        interactor)))))


(defun views-example ()
  (run-frame-top-level (make-application-frame 'views)))

;;; command to quit the application
(define-views-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-views-command (com-show-all :name t) ()
  ;; *standard-output* is bound the main pain, in particular we are
  ;; *setting the view inside of STANDARD-EXTENDED-OUTPUT-STREAM.
  (setf (stream-default-view *standard-output*) *members-view*))

(define-views-command (com-show-person :name t) ((person 'person-wraper))
  (if person
      (setf (stream-default-view *standard-output*)
            (make-instance 'person-view :person (person person)))
      (format (frame-standard-input *application-frame*)
              "Please give a person")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; First drawing try
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass file-trash ()
  ((%name      :initarg  :name
               :reader   name)
   (%directory :initarg  :directory
               :accessor directory-trash)))

(defun make-file (name)
  (make-instance 'file-trash :name name))

(defclass directory-trash (file-trash)
  ((%children :accessor children
              :initform '())))


(defclass trash (directory-trash)
  ()
  (:default-initargs :name "Trash"))

(defclass file-manager-view (view) ())

(define-presentation-method present ((object trash)
                                     (type   trash)
                                     (stream extended-output-stream)
                                     (view   file-manager-view)
                                     &key)
  (surrounding-output-with-border (stream :shape :rectangle :background +beige+)
    (print-members stream)
    (princ (name object) stream)
    (with-translation (stream (+ (stream-cursor-position stream) 8) 0)
      (draw-polygon* stream '(0 4 4 2 12 2 16 4 12 4 12 16 4 16 4 4)))))

(define-presentation-method present ((object person-wraper)
                                     (type   person-wraper)
                                     (stream extended-output-stream)
                                     (view   person-view)
                                     &key)
  (surrounding-output-with-border (stream :shape :rectangle :background +beige+)
    (display-pane-with-view 3 stream view)))

(defun print-members (stream)
  (formatting-item-list (stream)
    (map nil
         (lambda (child)
           (let ((wrap (make-instance 'person-wraper :person child)))
             (formatting-cell (stream)
               (present wrap (presentation-type-of wrap)
                        :stream stream
                        ;; changing default view from file-manager-view
                        :view (make-instance 'person-view :person child)))))
         *members*)))

(defun present-trash (obj stream)
  (formatting-item-list (stream)
    (present obj (presentation-type-of obj)
             :stream stream
             :single-box t)))
