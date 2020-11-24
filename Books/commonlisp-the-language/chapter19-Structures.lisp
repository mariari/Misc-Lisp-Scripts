(defstruct ship
  x-pos y-pos
  x-vel y-vel
  mass)

;; typep checks the type of an object

(ship-p 1)
(typep 1 'ship)

(defparameter ship2 (make-ship))

(setf ship2 (make-ship :mass 200
                       :x-pos 0
                       :y-pos 0))

(defparameter ship3 (copy-ship ship2))

(setf (ship-x-pos ship2) 100)


(defstruct person (name .007 :type string))
(make-person :name "James")

;; Errors out because .007 is not a  string
;; (make-person)


;;  types available for slot options

;; :type
;; :read-only
;; Impossible to specify a slot-option unless a default value is specified first

;;; Options

;;; conc-name

;; alternative prefix for freating the objects
;; if nil is used then the slot names are used without prefix x-pos

(defstruct (door (:conc-name nil)) knob-color width material)

(defparameter my-door (make-door :knob-color 'red :width 5.0))

(setf (knob-color my-door) 21)

(defstruct (door2 (:conc-name dr-)) knob-color width material)

(defparameter my-door2 (make-door2 :knob-color 'red :width 5.0))

(setf (dr-knob-color my-door2) 21)


;;; constructor

;; takes 1 arguments, the name of the constructor function

(defstruct (door3 (:constructor door3)
                  (:conc-name de-))
  knob-color width material)


(defparameter my-door3 (door3 :knob-color 'red :width 5.0))
(setf (de-knob-color my-door3) 21)
(copy-door3 my-door3)

(door3-p my-door3)

;;; copier

;; Changes the predicate for the copying function

;;; predicate
;;  names + -p

(defstruct (door4 (:constructor door4)
                  (:conc-name da-)
                  (:copier klown-door)
                  (:predicate doop))
  knob-color width material)


(defparameter my-door4 (door4 :knob-color 'red :width 5.0))
(setf (da-knob-color my-door4) 21)
(klown-door my-door4)
(doop my-door4)


;;; include

;; used to  create new extensions to an old structure 

;; (defstruct person2 name age sex)
(defstruct (astronaust2 (:include person)
                        (:conc-name astro-))
  helment-size (favorite-begerage 'tang))


(typep (make-astronaust2 :name "Beatrix") 'person)


;;; :print-function

;; special way for the function to be printed... can't be used with :type not to be confused with argument type


(defstruct (person2
             (:print-function
              ;;Special print function
              (lambda (p s k) (declare (ignore k))
                 (format s "<~A, sex ~A age ~D>"
                         (person2-name p)
                         (person2-sex p)
                         (person2-age p)))))
  (name "Dolores" :type string)
  age sex)


(defparameter my-person (make-person2 :name "beato" :age 11 :sex "female"))

(defstruct (hacker (:include person2)
                   (:conc-name hack-)
                   ;; (:print-function) ; would give the default structure notation if we did this 
                   )
  skillz sellout-lvl)

(make-hacker :name "alu" :skillz 10.2 :age 24 :sex "a dude" :sellout-lvl 8889)

;;; type
;; can't be chekced with type-p nor can use print-function

;;; named

;; by default all types without :type are named
;; takes no arguments 
(defstruct (hacker-v
             (:conc-name hack-)
             (:type list)
             :named)
  skillz-v sellout-lvl-v)

(make-hacker-v :skillz-v 0 :sellout-lvl-v (make-hacker-v :skillz-v 2))

;; #(HACKER-V 0 9899)



;;; inital-offset

;; cxan have more than 1 :consturctor 

(defstruct (foo (:constructor create-foo
                              (a &optional b (c 'sea) &rest d &aux e (f 'eff))))
  a b c d e f)

(create-foo 1 2 3 4 6 5 7 8 9 10)
;; #S(FOO :A 1 :B 2 :C 3 :D (4 6 5 7 8 9 10) :E 0 :F EFF)


(defstruct tree2 val left right)

(defparameter x (make-tree2 :val 1 :right (make-tree2 :val 2)))

(tree2-left x)


(defun make-tree-nodes (&rest values)
  (let ((x (make-tree2 :val (car values)))
        (values-arry (coerce (cdr values) 'vector)))
    (labels ((rec (root vals)
               (let ((right #2=(tree2-right root)) ; #2 to save this form later since we can't setf right and for it to work
                     (left  #1=(tree2-left root)))
                 (cond ((= (length vals) 0) root)
                       ((null left)
                        (setf #1# (make-tree2 :val (elt vals 0)))
                        (rec root (subseq vals 1)))
                       ((null right)
                        (let* ((split   (ceiling (1- (length vals)) 2))
                               (left-b  (subseq (subseq vals 1) 0 split))
                               (right-b (subseq (subseq vals 1)   split)))
                          (setf #2# (make-tree2 :val (elt vals 0)))
                        
                          (rec #1# left-b)
                          (rec #2# right-b)))))))
      (rec x values-arry))
    x))
