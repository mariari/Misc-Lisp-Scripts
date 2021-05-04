;;; BEING LAZY AND SET IN YOUR WAYS


;;; 6.2 designing a persistent toy

(use 'criterium.core)

(def baselist (list :denko :loli))

(def lst1 (cons :whiteline baselist))
(def lst2 (cons :arty baselist))

(= (next lst1) (next lst2))

(identical? (next lst1) (next lst2))

{:val 5, :L nil, :R nil}


;; (:val (xconj nil 5))

(defn xconj [tree val]
  (cond
    (nil? tree)         {:val val, :L nil, :R nil}
    (< val (:val tree)) {:val (:val tree)
                         :R   (:R   tree)
                         :L   (xconj (:L tree) val)}
    :else               {:val (:val tree)
                         :L   (:L   tree)
                         :R   (xconj (:R tree) val)}))


(def tree1 (xconj nil 5))

(def tree1 (xconj tree1 3))

(def tree1 (xconj tree1 1))

(defn xseq [tree]
  (when tree
    (concat (xseq (:L tree)) [(:val tree)] (xseq (:R tree)))))

(xseq tree1)

(def tree2 (xconj tree1 7))
(xseq tree2)

(identical? (:L tree1) (:L tree2))

(def tree3 (xconj tree1 2))

(def tree4 (xconj tree2 2))

(xseq tree3)
(xseq tree4)

(identical? (:R tree2) (:R tree4))      ; => T
(identical? (:L tree2) (:L tree4))      ; => F

;; We just create a new root node and the right child for the root node


(ns joy.q)

(def big-list (into [] (repeatedly 100000 #(rand-int 1000000))))

(defn nom [n] (repeatedly n #(rand-int n)))

(defn sort-parts [work]
  (lazy-seq
   (loop [[part & parts] work]
     (if-let [[pivot & xs] (seq part)]
       (let [smaller? #(< % pivot)]
         (recur (list* (filter smaller? xs)
                       pivot
                       (remove smaller? xs)
                       parts)))
       (when-let [[x & parts] parts]
         (cons x (sort-parts parts)))))))

(defn qsort [xs]
  (sort-parts (list xs)))


(defn qsrt [[pivot & xs]]
  (when pivot
    (lazy-cat (qsrt (filter #(< % pivot) xs))
              (filter #{pivot} xs)
              (qsrt (filter #(> % pivot) xs)))))

(defn qsrt-not-lazy [[pivot :as xs]]
  (when pivot
    (concat (qsrt (filter #(< % pivot) xs))
            (filter #(= % pivot) xs)
            (qsrt (filter #(> % pivot) xs)))))
