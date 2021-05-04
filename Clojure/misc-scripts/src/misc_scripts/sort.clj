(ns misc-scripts.sort
  (:use     [criterium.core :only (bench)])
  (:require [clojure.core.match :refer [match] :as m]))

;; Forward declarations---------------------------------------------------------------------------------------------
(declare insert)
;; Testing function-------------------------------------------------------------------------------------------------
(def big-list (into [] (repeatedly 100000 #(rand-int 1000000))))

;; Sorts------------------------------------------------------------------------------------------------------------

;; (bench (insertion-sort big-list))
;;1.58 ms or 1581000 ns
(defn insertion-sort
  "Insertion Sort pass <= for stable sorting greatest first"
  ([seq]     (reduce insert '() seq))
  ([seq cmp] (reduce (fn [x y] (insert x y cmp)) '() seq)))



;; (bench (qsort big-list))
;; 180.s
(defn qsort [[pivot & xs]]
  (when pivot
    (lazy-cat (qsort (filter #(< % pivot) xs))
              (cons pivot (filter #{pivot} xs))
              (qsort (filter #(> % pivot) xs)))))


;; helper functions-------------------------------------------------------------------------------------------------

;; (bench (insert (take 100000 (range 1 100000)) 10000000000))
;; 46ns for insert
;; 2.5e7ns for insert%
;; 45ns for insert%%

(defn insert
  "Inserts an element into a position that is > all previous elements
   can be changed based on the cmp function sent"
  ([seq ele]     (insert seq ele >=))
  ([seq ele cmp] (lazy-seq
                  (match seq
                         ([x :guard #(cmp % ele) & xs] :seq) (list* ele x xs)
                         ([x                     & xs] :seq) (cons x (insert xs ele))
                         ([]                           :seq) [ele]))))
(defn insert%%%
  ([seq ele]   (insert%%% seq ele >=))
  ([seq ele ≥] (loop [[x & xs] seq, acc []]
                 (cond (nil? x)  (conj acc ele)
                       (≥ x ele) (lazy-cat (conj acc ele x) xs)
                       :else     (recur xs (conj acc x))))))

(defn insert%%
  ([seq ele]     (insert seq ele >=))
  ([seq ele cmp] (let [[less more] (split-with #(cmp % ele) seq)]
                   (concat less [ele] more))))

(defn- insert% [seq ele]
  (loop [seq seq, acc []]
    (match seq
           ([x & xs] :seq) (if (> x ele)
                             (lazy-cat (conj acc ele x) xs)
                             (recur xs (conj acc x)))
           ([] :seq) (conj acc ele))))
