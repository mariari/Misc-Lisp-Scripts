(defpackage #:tutorial
  (:use #:clim-lisp #:clim))

(in-package :tutorial)

;;; -----------------------------------------------------------------------
;;; 1.2.5 First Attempt
;;; -----------------------------------------------------------------------

(define-application-frame first-clim-app ()
  ()
  ;; single pane
  (:panes
   (my-interactor
    :interactor
    :height 400
    :width  600))

  ;; :layouts section describes how the pane is positioned inside
  ;; the application frame.
  ;; With 1 pane, no point getting complicated, Default is fine...
  (:layouts
   (my-default my-interactor)))

;; Now that the structure of the app is defined, need a function
;; to launch an instance of this app. (The user could run
;; several instances of the same app.)
(defun run-my-first-app ()
  (run-frame-top-level (make-application-frame 'first-clim-app)))

;;; -----------------------------------------------------------------------
;;; 1.2.7 Adding Functionality
;;; -----------------------------------------------------------------------

(define-application-frame super-app ()
  ()
  (:pointer-documentation t)
  (:panes
   (app :application
        ;; When should this pane be displayed in the command loop.
        ;; Note that the refresh is pane-specific, not
        ;; application-wide.
        ;; since we don't store the data, don't clear every time
        :display-time nil
        :height 400
        :width  600)
   (int :interactor
        :height 200
        :width  600))
  (:layouts
   (default (vertically () app int))))

(define-super-app-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-super-app-command (com-parity :name t) ((number 'integer))
  (format t "~a is ~a~%" number
          (if (oddp number) "odd" "even")))

;; can declare this live
(define-super-app-command (com-barny :name t) ((number 'integer))
  (format t "~a is ~a~%" number
          (if (oddp number) "odd" "even")))

;; Don't need the com prefix!?
(define-super-app-command (what :name t) ((number 'integer))
  (format t "~a is ~a~%" number
          (if (oddp number) "odd" "even")))

(defun run-app ()
  (run-frame-top-level (make-application-frame 'super-app)))


;;; -----------------------------------------------------------------------
;;; 1.2.8 An application displaying a data structure
;;; -----------------------------------------------------------------------

(defpackage :tutorial-app
  (:use :clim :clim-lisp)
  (:export :app-main))

(in-package :tutorial-app)

(define-application-frame superapp ()
  ;; New addition of a slot to the application frame which
  ;; defines a application-specific slot.

  ;; The slot is simply a number.
  ((current-number :initform nil
                   :accessor current-number))
  (:pointer-documentation t)
  (:panes
   (app :application
        :height 400
        :width  600
        ;; default, the data is erased every time
        :display-time :command-loop
        :display-function #'display-app)
   (int :interactor
        :height 200
        :width  600))
  (:layouts
   (default (vertically () app int))))

(defun display-app (frame pane)
  (let ((number (current-number frame)))
    (format pane "~a is ~a"
            number
            (cond ((null number) "not a number")
                  ((oddp number) "odd")
                  (t             "even")))))

(define-superapp-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-superapp-command (com-parity :name t) ((number 'integer))
  (setf (current-number *application-frame*) number))

(defun app-main ()
  (run-frame-top-level (make-application-frame 'superapp)))

;;;; ----------------------------------------------------------------------
;;;; 1.3 Using incremental redisplay
;;;; ----------------------------------------------------------------------

;; Note we make some changes for efficiency and readability
;; 1. we don't use a list of lists, instead we use
;;    Vector (Ref Integers). This is because a reference is really
;;    what we want to guarantee uniquness and makes sense for the
;;    mutable code
;; 2. We change the loop into a =mapci= function which is just map
;;    with index, this is because loops are hard to read.
;; 3. We also abstracted the impure functions into pure counterparts
;;    with a very small amount of code dedicated to the mutation
;; Thus we have a dependency on my standard library for =vector:range=
;; and the =ref= module

(defpackage :tutorial-app-incremental
  (:use :clim :clim-lisp)
  (:local-nicknames (:ref :reference))
  (:export :app-main))

(in-package :tutorial-app-incremental)


(define-application-frame superapp ()
  ((numbers :initform
            ;; note the reference, this is important as the value has
            ;; to be unique boxing gives us the unique value.
            (coerce (loop :repeat 20 :collect (reference:ref (random 20)))
                    'vector)
            :accessor numbers)
   (cursor :initform 0 :accessor cursor))
  (:pointer-documentation t)
  (:panes
    (app :application
         :height 400 :width 600
         :incremental-redisplay t
         :display-function #'display-app)
    (int :interactor :height 200 :width 600))
  (:layouts
   (default (vertically () app int))))

;; As usual, the displaying code relates to a pane, not the application frame.
(defun display-app (frame pane)
  ;; taking items one-by-one from the frame slot 'numbers', along with
  ;; increasing the lines by the index
  (mapci
   (lambda (current-element line)
     ;; prints a star if the cursor is on that line
     ;; (Note that here, there is no incremental redisplay. The output
     ;; record of the star will be printed at each call of the display
     ;; function -- that is at each iteration of the command loop.)
     (princ (if (= (cursor frame) line) "*" " ") pane)
     ;; and incrementally updates the rendering instructions of the
     ;; number on that line
     ;; (Note that 'numbers' was defined as a list of lists, each
     ;; sublist holding an individual number. The reason for that is
     ;; explained below, but this is why (car current-element) is
     ;; needed.)
     (updating-output (pane :unique-id   current-element
                            :id-test     #'eq
                            :cache-value (ref:! current-element)
                            :cache-test  #'eql)
       (format pane "~a~%" (ref:! current-element))))
   (numbers frame)))

;; Helpers as I don't want to read loops
(defun mapci (f xs)
  "mapc with an extra index as an argument"
  (map nil f xs (vector:range 0 (length xs))))

;; --------------------------------------
;; Pure Helper Functions
;; --------------------------------------

(defun next (frame)
  "pure version of the move next command"
  (with-slots (cursor numbers) frame
    (if (= cursor (1- (length numbers)))
        0
        (1+ cursor))))

(defun previous (frame)
  "pure version of the move previous command"
  (with-slots (cursor numbers) frame
    (if (zerop cursor)
        (1- (length numbers))
        (1- cursor))))

;; --------------------------------------
;; Command definitions
;; --------------------------------------
;;

;; increase the value of the number on the current line
(define-superapp-command (com-add :name t) ((number 'integer))
  (let ((current-element (elt (numbers *application-frame*)
                              (cursor *application-frame*))))
    (setf (ref:! current-element)
          (+ (ref:! current-element) number))))

;; move the cursor one line down (increasing the cursor position),
;; looping back to the beginning if going too far
(define-superapp-command (com-next :name t) ()
  (setf (cursor *application-frame*)
        (next *application-frame*)))


;; move the cursor one line up
(define-superapp-command (com-previous :name t) ()
  (setf (cursor *application-frame*)
        (previous *application-frame*)))

;; Command to quit the app
(define-superapp-command (com-quit :name t) ()
  (frame-exit *application-frame*))


;; Exported function to launch an instance of the application frame
(defun app-main ()
  (run-frame-top-level (make-application-frame 'superapp)))

;;;;; ----------------------------------------------------------------------
;;;;; 1.4.0 Presentation Types
;;;;; ----------------------------------------------------------------------

;;;; ----------------------------------------------------------------------
;;;; 1.4.2 A simple example
;;;; ----------------------------------------------------------------------

(defpackage :tutorial-app-presentation
  (:use :clim :clim-lisp)
  (:export :app-main))

(in-package :tutorial-app-presentation)

(define-application-frame superapp ()
  ()
  (:pointer-documentation t)
  (:panes
    (app :application :display-time t :height 300 :width 600)
    (int :interactor :height 200 :width 600))
  (:layouts
    (default (vertically () app int))))

(defun app-main ()
  (run-frame-top-level (make-application-frame 'superapp)))

(define-superapp-command (com-quit :name t) ()
  (frame-exit *application-frame*))

(define-presentation-type name-of-month ()
  :inherit-from 'string)

(define-presentation-type day-of-month ()
  :inherit-from 'integer)

(define-superapp-command (com-out :name t) ()
  (with-output-as-presentation (t "The third month" 'name-of-month)
    (format t "March~%"))
  (with-output-as-presentation (t 15 'day-of-month)
    (format t "fifteen~%")))

;; This is a bit clunky to get the input to be considerd valid
;; presentations
(define-superapp-command (com-get-date :name t)
    ((name 'name-of-month) (date 'day-of-month))
  (let ((input-stream (frame-standard-input *application-frame*)))
    (format input-stream "the ")
    (with-output-as-presentation (input-stream date 'day-of-month)
      (format input-stream "~R" date))
    (format input-stream " of ")
    (with-output-as-presentation (input-stream name 'name-of-month)
      (format input-stream "~A~%" name))))

(define-superapp-command (com-old-get-date :name t)
    ((name 'name-of-month) (date 'day-of-month))
  (format (frame-standard-input *application-frame*)
          "the ~a of ~a~%" date name))

;;;;; ----------------------------------------------------------------------
;;;;; 1.5.0 Using Views
;;;;; ----------------------------------------------------------------------


(defpackage :tutorial-app-views
  (:use :clim :clim-lisp)
  (:export :app-main))

(in-package :tutorial-app-views)

;;; part of application "business logic"
(defclass person ()
  ((%last-name :initarg :last-name :accessor last-name)
   (%first-name :initarg :first-name :accessor first-name)
   (%address :initarg :address :accessor address)
   (%membership-number :initarg :membership-number :reader membership-number)))

;;; constructor for the PERSON class.  Not strictly necessary.
(defun make-person (last-name first-name address membership-number)
  (make-instance 'person
                 :last-name last-name
                 :first-name first-name
                 :address address
                 :membership-number membership-number))

;;; initial list of members of the organization we imagine for this example
(defparameter *members*
  (list (make-person "Doe" "Jane" "123, Glencoe Terrace" 12345)
        (make-person "Dupont" "Jean" "111, Rue de la Republique" 54321)
        (make-person "Smith" "Eliza" "22, Trafalgar Square" 121212)
        (make-person "Nilsson" "Sven" "Uppsalagatan 33" 98765)))

;;; the CLIM view class that corresponds to a list of members, one member
;;; per line of text in a CLIM application pane.
(defclass members-view (view) ())

;;; since this view does not take any parameters in our simple example,
;;; we need only a single instance of it.
(defparameter *members-view* (make-instance 'members-view))

;;; the application frame.  It contains instance-specific data
;;; such as the members of our organization.
(define-application-frame views ()
  ((%members :initform *members* :accessor members))
  (:panes
   (main-pane :application :height 500 :width 500
              :display-function #'display-main-pane
              ;; notice the initialization of the default view of
              ;; the application pane.
              :default-view *members-view*)
   (interactor :interactor :height 100 :width 500))
  (:layouts
   (default (vertically ()
              main-pane
              interactor))))

;;; the trick here is to define a generic display function
;;; that is called on the frame, the pane AND the view,
;;; whereas the standard CLIM display functions are called
;;; only on the frame and the pane.
(defgeneric display-pane-with-view (frame pane view))

;;; this is the display function that is called in each iteration
;;; of the CLIM command loop.  We simply call our own, more elaborate
;;; display function with the default view of the pane.
(defun display-main-pane (frame pane)
  (display-pane-with-view frame pane (stream-default-view pane)))

;;; now we can start writing methods on our own display function
;;; for different views.  This one displays the data each member
;;; on a line of its own.
(defmethod display-pane-with-view (frame pane (view members-view))
  (mapc (lambda (member)
          (with-output-as-presentation (pane member 'person)
            (format pane "~a, ~a, ~a, ~a~%"
                    (membership-number member)
                    (last-name member)
                    (first-name member)
                    (address member))))
        (members frame)))

;;; this CLIM view is used to display the information about
;;; a single person.  It has a slot that indicates what person
;;; we want to view.
(defclass person-view (view)
  ((%person :initarg :person :reader person)))

;;; this method on our own display function shows the detailed
;;; information of a single member.
(defmethod display-pane-with-view (frame pane (view person-view))
  (let ((person (person view)))
    (format pane "Last name: ~a~%First Name: ~a~%Address: ~a~%Membership Number: ~a~%"
            (last-name person)
            (first-name person)
            (address person)
            (membership-number person))))

;;; entry point to start our applciation
(defun views-example ()
  (run-frame-top-level (make-application-frame 'views)))

;;; command to quit the application
(define-views-command (com-quit :name t) ()
  (frame-exit *application-frame*))

;;; command to switch the default view of the application pane
;;; (which is the value of *standard-output*) to the one that
;;; shows a member per line.
(define-views-command (com-show-all :name t) ()
  ;; *standard-output* is bound the main pain, in particular we are
  ;; *setting the view inside of STANDARD-EXTENDED-OUTPUT-STREAM.
  (setf (stream-default-view *standard-output*) *members-view*))

;; Slot value of *standard-ouptut*

;; STANDARD-EXTENDED-OUTPUT-STREAM:
;; ...
;; [ ]  BASELINE                   = 13307/1024
;; [ ]  CHAR-HEIGHT                = 0
;; [ ]  CURSOR                     = #<STANDARD-TEXT-CURSOR 0 1171/16  {10095696C3}>
;; [ ]  EOL                        = :WRAP
;; [ ]  EOP                        = :SCROLL
;; [ ]  VIEW                       = #<MEMBERS-VIEW {10015A51D3}>
;; [ ]  VSPACE                     = 2


;;; command to switch to a view that displays a single member.
;;; this command takes as an argument the person to display.
;;; In this application, the only way to satisfy the demand for
;;; the argument is to click on a line of the members view.  In
;;; more elaborate application, you might be able to type a
;;; textual representation (using completion) of the person.
(define-views-command (com-show-person :name t) ((person 'person))
  (if person
      (setf (stream-default-view *standard-output*)
            (make-instance 'person-view :person person))
      (format (frame-standard-input *application-frame*)
              "Please give a person")))

(define-views-command (com-dump-sto :name t) ()
  (defparameter *x* *standard-output*)
  (format (frame-standard-input *application-frame*)
              "*Standard-output* ~A" *standard-output*))
