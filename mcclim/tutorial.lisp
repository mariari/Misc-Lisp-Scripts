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

;;;; ----------------------------------------------------------------------
;;;; 1.4.0 Presentation Types
;;;; ----------------------------------------------------------------------
