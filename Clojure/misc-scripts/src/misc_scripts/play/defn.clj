(require '[clojure.core.match :refer [match] :as m]
         '[defun.core :refer [defun] :as d]
         ;; '[clojure.set]
         '[clojure.tools.trace :as c]
         ;; '[clojure.algo.monads :as mo]
         )


(use 'clojure.set)
(use 'clojure.algo.monads)
;; (use 'clojure.algo.moands)



;; memoized version of
(def m-fib
  (memoize
   (fn [n]
     (condp = n
       0 1
       1 1
       (+' (m-fib (dec n)) (m-fib (- n 2)))))))

;; case is O(1) unlike condp which is sequential

(def m-fib-case
  (memoize
   (fn [n]
     (case n
       0 1
       1 1
       (+' (m-fib-case (dec n)) (m-fib-case (- n 2)))))))



;;  slower since we need to pass f every time
(defn fib [n]
  (let [fib-in
        (memoize (fn [f n]
                   (case n
                     0 1
                     1 1
                     (+' (f f (dec n)) (f f (- n 2))))))]
    (fib-in fib-in n)))

(defn fib-step [[a b]]
  [b (+' a b)])

(defn fib-seq []
  (map first (iterate fib-step [0 1])))

(take 20 (fib-seq))


(defn plot [f min max step]
  (doseq [i (range min max step)]  ; same as for... but for side effects
    (dotimes [_ (f i)]             ; dotimes is also used for side effects
      (print "*"))
    (println)))


(plot #(Math/pow % 2) 1 5 1)

(plot identity 2 10 2)


(call-cc (fn [x] (map #(if (= % 4)
                         (x (* % %)))
                      (range 20))))

(def continuation nil)

(run-cont
  (domonad cont-m
    [x (m-result 1)
     y (call-cc (fn [c] (def continuation c) (c 2)))]
    (+ x y)))



;; (run-cont (sqrt-as-str 2))
;; (run-cont (sqrt-as-str -2))

(run-cont (continuation 5))


(call-cc (fn [x] (map #(if (= % 4)
                         (x (* % %)))
                      (range 20))))

(domonad (maybe-t sequence-m)
         [x (for [n (range 10)] (when (odd? n) n))]
         (inc x))

(domonad (maybe-t sequence-m)
         [x (map #(when (odd? %) %) (range 10))]
         (inc x))
