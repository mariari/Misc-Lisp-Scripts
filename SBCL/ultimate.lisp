;;; Following
;;; https://pvk.ca/Blog/2014/03/15/sbcl-the-ultimate-assembly-code-breadboard/
;;; SBCL: the ultimate assembly code breadboard

(defpackage #:stack
  (:documentation "Small Stack VM in SBCL")
  (:use #:common-lisp))

(in-package :stack)

;; SBCL version 2.1.0 renames make-ea into ea
(import '(sb-assem:inst sb-vm::ea))

(defvar *stack*
  (make-array 8
              :initial-contents (list sb-vm::r8-tn
                                      sb-vm::r9-tn
                                      sb-vm::r10-tn
                                      sb-vm::r11-tn
                                      sb-vm::r12-tn
                                      sb-vm::r13-tn
                                      sb-vm::r14-tn
                                      sb-vm::r15-tn)))
(defvar *stack-pointer*)

(defun @ (i)
  "(@ 0) returns the (current) register for top of stack (TOS),
(@ 1) returns the one just below, etc."
  (aref *stack* (mod (+ i *stack-pointer*) (length *stack*))))

(defvar *code-base* sb-vm::rsi-tn)
(defvar *virtual-ip* sb-vm::rdi-tn "virtual instruction pointer")

(defvar *rax* sb-vm::rax-tn)
(defvar *rbx* sb-vm::rbx-tn)
(defvar *rcx* sb-vm::rcx-tn)
(defvar *rdx* sb-vm::rdx-tn)

(defvar *primitive-code-offset* (* 64 67))

(defstruct code-page
  (alloc 0)
  (code (make-array *primitive-code-offset* :element-type '(unsigned-byte 8))))

(defun emit-code (pages emitter)
  ;; there must be as many code pages as there are stack slots
  (assert (= (length *stack*) (length pages)))
  ;; find the rightmost starting point, and align to 16 bytes
  ;; not 15 = -16 = 1...10000
  ;; 15 = 0000...1111
  ;; Thus every value is a factor of 16
  (let* ((alloc (logandc2 (+ 15 (reduce #'max pages :key #'code-page-alloc))
                          15))
         (bytes (map-with-index
                 (lambda (page i)
                   (let ((segment (sb-assem:make-segment))
                         (*stack-pointer* i))
                     ;; assemble the variant for this value of
                     ;; *stack-pointer* in a fresh code segment
                     (sb-assem:assemble (segment)
                       ;; pad out the segment by aligning to 16 bytes
                       (sb-vm:emit-long-nop segment (- alloc (code-page-alloc page)))
                       (funcall emitter))
                     ;; tidy up backreference
                     (sb-assem:finalize-segment segment)
                     ;; then get the (position-independent) machine
                     ;; code as a vector of bytes
                     (sb-assem:segment-contents-as-vector segment)))
                 pages)))
    ;; finally, copy each machine code sequence to the right code page
    (map nil
         (lambda (page bytes)
           (let ((alloc (code-page-alloc page)))
             (replace (code-page-code page) bytes :start1 alloc)
             (assert (<= (+ alloc (length bytes)) (length (code-page-code page))))
             (setf (code-page-alloc page) (+ alloc (length bytes)))))
         pages bytes)
    ;; and return the offset for that code sequence
    alloc))

;; if we make this with a loop it's likely faster
(defun mapi (result-type f sequence)
  "acts like map but also grabs the index"
  (map result-type f sequence (vector:range 0 (length sequence))))

;; faster version but for arrays only
(defun map-with-index (f vector)
  "acts like map but also grabs the index"
  (loop for i below (length vector)
        for page = (elt vector i)
        collect (funcall f page i)))
