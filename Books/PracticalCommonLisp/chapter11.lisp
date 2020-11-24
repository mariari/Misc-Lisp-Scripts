;;;; Collectibles--------------------------------------------------------------------------------------------------------------------------------------

;;; Vectors------------------------------------------------------------------------

;; Make fixed size vectors of dimension 1 
(vector 1)

;; Makes a vector with inital elements
(make-array 5 :initial-element nil)
(make-array 5)



;; Pusing and piping from the end of the array
(defparameter *x* (make-array 5 :fill-pointer 0))

(vector-push 'a *x*)
(vector-pop  *x*)


;; Use adjustable if you want it to be able to adjust 
 (make-array 5 :fill-pointer 0 :adjustable t)
(vector-push-extend  'a (make-array 5 :fill-pointer 0 :adjustable t))


;; Make a re-adjustable string
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'character)

;; Bit vector
(make-array 5 :fill-pointer 0 :adjustable t :element-type 'bit)
#*101001



;; Generic sequence modifiers

(count "foo" #("foo" "bar" "baz") :test #'string=)

(find 'c #((a 10) (b 20) (c 30) (d 40)) :key #'car)

(remove #\a "foobarbaz" :count 1 :from-end t)



;; Higher order parameters
(count-if-not #'evenp #((1 a) (2 b) (3 c) (4 d) (5 e)) :key #'car)


(mismatch "foobar" "bar" :from-end t)
