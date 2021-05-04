(ns misc-scripts.functions
  (:use (misc-scripts.sort)
        (misc-scripts.macros)))

(require '[clojure.core.match :refer [match] :as m])

;; lazy seq makes this way slower
(letfn [(max-count [xs ys] (if (> (count xs) (count ys)) xs ys))]
  (def lcs
    (memoize
     (fn [[x & xs :as lis1] [y & ys :as lis2]]
       (cond (not (and x y)) nil
             (= x y)         (cons x (lcs xs ys))
             :else           (max-count (lcs xs lis2) (lcs lis1 ys)))))))

(defn map%
  ([f]    #(map% f %))
  ([f xs] (if (empty? xs)
              nil
              (cons (f (first xs)) (map% f (rest xs))))))

(defn foldr'
  ([f z xs]     (foldr' f z xs identity))
  ([f z xs cps] (match xs ([x & ys] :seq) (recur f z ys (fn [y] #(cps (f x y))))
                          ([] :seq)       (trampoline cps z))))
(defn foldr''
  ([f z xs]              (foldr'' f z xs identity))
  ([f z [head & xs] cps] (cond head  (recur f z xs (fn [y] #(cps (f head y))))
                               :else (trampoline cps z))))


(defn gen-dispatch [struct]
  "provides CL like dispatch to defmulti"
  (if (keyword? struct) struct (type struct)))
