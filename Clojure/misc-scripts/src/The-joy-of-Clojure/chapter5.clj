(require '[clojure.core.match :refer [match] :as m]
         '[defun.core :refer [defun] :as d]
         ;; '[clojure.set]
         '[clojure.tools.trace :as c]
         '[clojure.algo.monads :as mo]
         )

(def ds (into-array [:while :barnabas :adam]))

(seq ds)

;; Change the array
(aset ds 1 :quetin)

;; the 2nd object is now quetin
(seq ds)



(into (vector-of :int) [Math/PI 2 1.3])

(def a-to-j (vec (map char (range 65 75))))

(nth a-to-j 4)
(get a-to-j 4)
(a-to-j 4)

(assoc a-to-j 4 "no longer E")

(replace {2 :a 4 :b} [0 1 2 3 4])


(def matrix
  [[1 2 3]
   [4 5 6]
   [7 8 9]])

(get-in matrix [1 2])

(assoc-in matrix [1 2] 'x)


(update-in matrix [1 2] * 100)

(defn neighbors
  ([size yx]
   (neighbors [[-1 0] [1 0] [0 -1] [0 1]] size yx))
  ([deltas size yx]
   (filter (partial every? #(< -1 % size))
           (map #(map + yx %) deltas))))


(def my-stack [1 2 3])
(def my-stack-2 '(1 2 3))

(peek my-stack)
(peek my-stack-2)

(pop my-stack)
(pop my-stack-2)

(conj my-stack 4)

(defn strict-map2 [f coll]
  (loop [[a & bs] coll, acc []]
    (if-not a
      acc
      (recur bs (conj acc (f a))))))

(defn strict-map2% [f coll]
  (loop [coll coll, acc []]
    (if (empty? coll)
      acc
      (recur (rest coll)
             (conj acc (f (first coll)))))))

(def schedule
  (conj clojure.lang.PersistentQueue/EMPTY
        :wake-up :shower :brush-teeth))

(defmethod print-method clojure.lang.PersistentQueue
  [q, w]
  (print-method '<- w) (print-method (seq q) w) (print-method '-< w))

;; queue has 2 data structures seq at front and a vector in the back


;; remember to use pop, peek, etc etc to return a queue instead of a sequence

#{'(1 2) [1 2]}


;; There are sorted sets
;;  kinda annoying as the objects have to be comparable

;; can offer your own comparer operator 
(sorted-set :b :a :c)

(sorted-set [1 5] [2 1])

(into {} [[:a 1] [:b 2]])

(apply hash-map [:a 1 :b 2])

;;; 5.7 Putting it all together---------------------------------------------------------------------


(defn pos% [ele coll]
  (let [cmp (if (map? coll)
              #(= (second %1) %2)
              #(= %1 %2))]
    (loop [s coll, acc 0]
      (when (seq s)
        (if (cmp (first s) ele)
          (if (map? coll)
            (first (first s))
            acc)
          (recur (rest s) (inc acc)))))))

(defn our-index [coll]
  (cond (map? coll) (seq coll)
        (set? coll) (map vector coll coll)
        :else       (map vector (iterate inc 0) coll)))
(defn our-index [coll]
  (cond (map? coll) (seq coll)
        (set? coll) (map vector coll coll)
        :else       (map-indexed vector coll)))

(our-index [:a 1 :b 2 :c 3 :d 4])
(our-index {:a 1 :b 2 :c 3 :d 4})
(our-index #{:a 1 :b 2 :c 3 :d 4})


(defn pos%% [ele coll]
  (for [[i v] (our-index coll) :when (= ele v)] i))


(pos%% 3 {:a 1, :b 2, :c 3, :d 4})
(pos%% 3 [:a 1, :b 2, :c 3, :d 4])

;; pos%% was useful but we want to do something like this

(defn pos [pred coll]
  (for [[i v] (our-index coll) :when (pred v)] i))


;; #{3 4} is a cool pred because we can do this (#{3 4} 3) -> 3
(pos #{3 4} {:a 1 :b 2 :c 3 :d 4})

(pos even? [2 3 6 7])


;; The problem with & is that it's nthnext
;; nthnext is less lazy than rest
(defn rec-step [[x & xs]]
  (if x
    [x (rec-step xs)]
    [               ]))

(defn lz-rec-step [s]
  (lazy-seq
   (if (seq s)
     [(first s) (lz-rec-step (rest s))]
     [                                ])))

(defn lz-rec-step-test [[x & xs]]
  (lazy-seq
   (if x
     [x (lz-rec-step-test xs)]
     [                       ])))
;; this version does not crash
(dorun (lz-rec-step (range 200000)))


(defn simple-range [i limit]
  (lazy-seq
   (when (< i limit)
     (cons i (simple-range (inc i) limit)))))

(defn simple-range-tco
  ([i limit]     (simple-range-tco i limit []))
  ([i limit acc] (if (>= i limit)
                   acc
                   (simple-range-tco (inc i) limit (conj acc i)))))

;; (time (dorun (simple-range 0 20000000)))
;; (time (dorun (simple-range-tco 0 20000000)))


(take 20 (iterate (fn [n] (/ n 2)) 1))


(defn triangle [n]
  (/ (* n (+ n 1)) 2))

(map triangle (range 1 11))

(def tri-nums (map triangle (iterate inc 1)))

(take 10 (filter even? tri-nums))

(double (reduce + (take 1000 (map / tri-nums))))

(nth tri-nums 99)

(take 2 (drop-while #(< % 10000) tri-nums))

(time (take 2 (drop-while (fn [[a b]] (< b 10000)) (our-index tri-nums))))
(time (take 2 (drop-while #(< (second %) 10000) (map-indexed vector tri-nums))))
