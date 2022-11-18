(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; netfarm won't load if you don't have it on your path since it's
  ;; not on quicklisp the second quickload is harder to satisfy need
  ;; to grab a lot more software. The calls to some of the software
  ;; deadlocks so I've commented out related code.

  ;; (ql:quickload '(:serapeum :netfarm))
  ;; (ql:quickload '(:netfarm-client :netfarm-server))
  )

(defpackage #:netfarm-playground
  (:use :common-lisp :serapeum))

(in-package #:netfarm-playground)

(defclass foo-class ()
  ((a :initarg :a)
   (b :initarg :b)
   (c :initarg :c))
  (:metaclass netfarm:netfarm-class))

(defmethod print-object ((obj foo-class) stream)
  (flet ((bound (x)
           (if (slot-boundp obj x) (slot-value obj x) "#<unbound>")))
    (pprint-logical-block (stream nil)
        (print-unreadable-object (obj stream :type t)
          (format stream "~3I~_:a ~W ~_:b ~W ~_:c ~A"
                  (bound 'a)
                  (bound 'b)
                  (bound 'c))))))

(def foo-fields
  (c2mop:class-slots (find-class 'foo-class)))

(def object-to-play
  (make-instance 'foo-class
                 :a "This is A"
                 :b '("This" "is" 2)
                 :c #(1 2 3 4 5 6)))

(def rendered
  (netfarm:render-object object-to-play))

(def parsed-into-data
  (netfarm:parse-block rendered))

(def object-back
  (netfarm:apply-class (netfarm:parse-block rendered)
                       (find-class 'foo-class)))

(def bigger-object
  (make-instance
   'foo-class
   :a "Hi there"
   :b nil
   :c (make-instance 'foo-class
                     :a nil
                     :b "How are you?"
                     :c (make-instance 'foo-class
                                       :a "I'm doing fine"
                                       :b nil
                                       :c nil))))

;; (defvar *system*
;;   (make-instance 'netfarm-server:memory-system))

;; (decentralise-system:start-system *system*)

;; (defparameter *client*
;;   (decentralise-client:connect-to-system *system*))

;; ;; lets start saving things
;; ;; Times out for me
;; (netfarm-client:save-object *client* bigger-object)
