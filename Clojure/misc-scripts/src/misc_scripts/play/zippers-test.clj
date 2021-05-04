(use  '[clojure.zip :as z])


(def data [1
           [2 3 4]
           [[5 6]
            [7 8]
            [[9 10 11]]]
           12])



(-> (z/vector-zip data)
   (z/down)
   (z/right)
   (z/right)
   (z/down)
   (z/replace 42)
   (z/next))


(defn zip-map [f xs]
  (loop [xs xs]
    (cond (identical? (z/next xs) xs) (z/root xs)
          (z/branch? xs)              (recur (z/next xs))
          :else                       (recur (-> xs (z/edit f) z/next)))))

(defn zip-foldl [f z xs]
  (zip-foldl-elem #(f %1 (z/node %2)) z xs))

(defn zip-foldl-elem [f z xs]
  (loop [xs xs acc z]
    (cond (identical? (z/next xs) xs) acc
          (z/branch? xs)              (recur (z/next xs) acc)
          :else                       (recur (z/next xs) (f acc xs)))))

(defun zip-map' [f xs]
  (zip-foldl-elem (fn [xs x] )))