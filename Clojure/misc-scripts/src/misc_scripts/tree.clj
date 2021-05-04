(ns misc-scripts.tree
  (:use [misc-scripts.functions]))

(require '[cats.builtin :as b]
         '[cats.core :as c]
         '[clojure.core.match :refer [match] :as m]
         '[lambdaisland.uniontypes :refer [case-of]]
         '[clojure.core.reducers :as r]
         ;; '[clojure.spec.alpha :as s] ; is not needed, as we can use case-of instead of s/conform
         )
(use '[misc-scripts.macros])

;; settled way of doing things with clojure 1.9==============================================
(defadt tree t
  (Node-a   [(l tree?) ele (r tree?)])
  (Single-a [ele])
  (Empty-a))

(declare insert)
(letfn [(insert-node [x {ele :ele :as node}]
          (update node (if (> x ele) :r :l) #(insert x %)))]

  (defn insert [x tree]
    (case-of :t/tree tree
      :t/Empty-a  {}         (Single-a. x)
      :t/Node-a   {}         (insert-node x tree)
      :t/Single-a {ele :ele} (insert-node x (Node-a. (Empty-a.) ele (Empty-a.))))))

;; Settled way of doing things without clojure 1.9===========================================
(defrecord Node [l ele r])
(defrecord Single [ele])
(def empty :empty-b)

(defmethod print-method Node [{l :l e :ele r :r} ^java.io.Writer w]
  (.write w (str [:Node {:l l} {:ele e} {:r r}])))

(defmethod print-method Single [{e :ele} ^java.io.Writer w]
  (.write w (str [:Single {:ele e}])))
;; IDEAL WAY not clojure 9====================================================================
(defmulti insert-rm (fn [x tree] (gen-dispatch tree)))

(letfn [(insert-node [x {ele :ele :as node}]
          (update node (if (> x ele) :r :l) #(insert-rm x %)))]

  (defmethod insert-rm empty  [x empty]    (Single. x))
  (defmethod insert-rm Single [x {e :ele}] (insert-node x (Node. empty e empty)))
  (defmethod insert-rm Node   [x n]        (insert-node x n)))

;; LESS OPTIMAL WAY==============================================================
(defmulti insert-rm% (fn [x tree] (if (keyword? tree) tree (type tree))))

(defmethod insert-rm% empty  [x empty]              (Single. x))
(defmethod insert-rm% Single [x {e :ele}]           (cond (> x e) (Node. empty e (Single. x))
                                                          :else   (Node. (Single. x) e empty)))
(defmethod insert-rm% Node   [x {l :l e :ele r :r}] (cond (> x e) (Node. l e (insert-rm% x r))
                                                          :else   (Node. (insert-rm% x l) e r)))
;; Pattern matching kinda sucks in Clojure
;; We can't even say the type of the thing, we must just use map syntax
(defn insert-r [x xs]
  (match xs
     :empty-b                             (Single. x)
     {:l l :ele (e :guard #(> x %)) :r r} (Node. l e (insert-r x r))
     {:l l :ele e :r r}                   (Node. (insert-r x l) e r)
     {:ele (e :guard #(> x %))}           (Node. empty e (Single. x))
     {:ele e}                             (Node. (Single. x) e empty)))

;; Previous failed attempts using different tools================================

(deftype Branch  [left ele right])
(deftype Element [ele])
(def empty :empty-b)

;; (defn element? [x]
;;   (instance? Element x))

(defn empty-b? [x]
  (= :empty-b x))

(defmethod print-method Branch [v ^java.io.Writer w]
  (.write w (str [:Branch (.left v) (.ele v) (.right v)])))

(defmethod print-method Element [v ^java.io.Writer w]
  (.write w (str [:Element (.ele v)])))


;; (defn insert-t [x xs]
;;   (cond (empty-b? xs) (Element. x)
;;         (element? xs) (if (< x (.ele xs))
;;                           (Branch. (Element. x) (.ele xs) empty)
;;                           (Branch. empty (.ele xs) (Element. x)))
;;         (branch? xs)  (if (< x (.ele xs))
;;                           (Branch. (insert-t x (.left xs)) (.ele xs) (.right xs))
;;                           (Branch. (.left xs)              (.ele xs) (insert-t x (.right xs))))))


;; (let [x (vec (doall (range 1000000)))]
;;   (time (r/fold + x))
;;   (time (r/reduce + x))
;;   (time (reduce + x)))
