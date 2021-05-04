;; (use '[clojure.test :as t])

(map [:chthon :phthor :beowulf :grendel] #{0 3})

;; (clojure.test/run-tests)

(defn join
  {:test (fn []
           (assert
            (= (join "," [1 2 3]) "1,2,3")))}
  [sep s]  (apply str (interpose sep s)))


(def plays [{:band "Burial",     :plays 979,    :loved 9}
            {:band "Eno",        :plays 2333,   :loved 15}
            {:band "Bill Evans", :plays 979,    :loved 9}
            {:band "Magma",      :plays 2665,   :loved 31}])


(defn keys-apply [f ks m]
  "Takes a function, a set of keys, and a map and applies the function
   to the map on the given keys. A new map of the results of the function
    applied to the keyed entries is returned."
  (let [only (select-keys m ks)]
    (zipmap (keys only) (map f (vals only)))))

(keys-apply #(.toUpperCase %) #{:band} (plays 0))
(map (partial keys-apply #(.toUpperCase %) #{:band}) plays)


(defn manip-map [f ks m]
  (conj m (keys-apply f ks m)))

(manip-map #(int (/ % 2)) #{:plays :loved} (plays 0))


(defn halve! [ks]
  (map (partial manip-map #(int (/ % 2)) ks) plays))

(halve! [:plays])

(defn slope
  [& {:keys [p1 p2] :or {p1 [0 0] p2 [1 1]}}]
  (float (/ (- (p2 1) (p1 1))
            (- (p2 0) (p1 0)))))

(slope :p1 [4 15] :p2 [3 21]) ;=> -6.0
(slope :p2 [2 1])             ;=> 0.5
(slope)                       ;=> 1.0


(defn slope2 [p1 p2]
  {:pre [(not= p1 p2) (vector? p1) (vector? p2)]
   :post [(float? %)]}
  (/ (- (p2 1) (p1 1))
     (- (p2 0) (p1 0))))
(def add-and-get
  (let [ai (java.util.concurrent.atomic.AtomicInteger.)]
    (fn [y] (.addAndGet ai y))))


(def bearings [{:x 0  :y 1}
               {:x 1  :y 0}
               {:x 0  :y -1}
               {:x -1 :y 0}])

(defn bot [x y bearing-num]
  {:coords [x y]
   :bearing   ([:north :east :south :west] bearing-num)
   :forward    (fn [] (bot (+ x (:x (bearings bearing-num)))
                          (+ y (:y (bearings bearing-num)))
                          bearing-num))
   :turn-right (fn [] (bot x y (mod (+ 1 bearing-num) 4)))
   :turn-left  (fn [] (bot x y (mod (- 1 bearing-num) 4)))})



(:coords (bot 5 5 0))
(:bearing (bot 5 5 0))
(:coords ((:forward (bot 5 5 0))))

(defn mk-cps [accept? end-value kend kont]
  (fn [n]
    ((fn [n k]
       (let [cont (comp k #(kont % n))]
         (if (accept? n)
           (k end-value)
           (recur (dec n) cont))))
     n kend)))

(def fac (mk-cps zero? 1 identity #(* %1 %2)))

(def tri (mk-cps zero? 1 dec #(+ %1 %2)))


(def world [[1   1   1   1]
            [999 999 999 1]
            [1   1   1   1]
            [1 999 999 999]])
