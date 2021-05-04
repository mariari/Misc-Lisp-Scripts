(defn run [nvecs nitems nthreads niters]
  (let [vec-refs (mapv (comp ref vec)
                       (partition nitems (range (* nvecs nitems))))
        swap #(let [v1 (rand-int nvecs)
                    v2 (rand-int nvecs)
                    i1 (rand-int nitems)
                    i2 (rand-int nitems)]
                (dosync
                 (let [temp (nth @(vec-refs v1) i1)]
                   (alter (vec-refs v1) assoc i1 (@(vec-refs v2) i2))
                   (alter (vec-refs v2) assoc i2 temp))))
        report #(do ;; (prn (map deref vec-refs))
                  (println "Distinct:"
                           (count (distinct (apply concat (map deref vec-refs))))))]
    (report)
    (dorun (apply pcalls (repeat nthreads #(dotimes [_ niters] (swap)))))
    (report)))

(import '(java.util.concurrent Executors))

(defn test-stm [nitems nthreads niters]
  (let [refs  (map ref (repeat nitems 0))
        pool  (Executors/newFixedThreadPool nthreads)
        tasks (map (fn [t]
                     #(dotimes [_ niters]
                        (dosync
                         (doseq [r refs]
                           (alter r + 1 t)))))
                   (range nthreads))]
    (doseq [future (.invokeAll pool tasks)]
      (.get future))
    (.shutdown pool)
    (map deref refs)))

(defn test-stm% [nitems nthreads niters]
  (let [refs  (map ref (repeat nitems 0))
        tasks (map (fn [t]
                     #(dotimes [_ niters]
                       (dosync
                        (doseq [r refs] (alter r + 1 t)))))
                   (range nthreads))]
    (dorun (apply pcalls tasks))
    (map deref refs)))


(defn longest-prefix
  "Finds the longest prefix between two lists using a comparison function"
  ([xs ys]     (longest-prefix xs ys >))
  ([xs ys cmp] (->> ys
                   (map vector xs)
                   (take-while (fn [[x y]] (cmp x y)))
                   (map first))))


(defn relay [x i]
  (when (:next x)
    (send (:next x) relay i))
  (when (and (zero? i) (:report-queue x))
    (.put (:report-queue x) i))
  x)

(defn run [m n]
  (let [q  (new java.util.concurrent.SynchronousQueue)
        hd (nth (iterate #(agent {:next %})
                         (agent {:report-queue q}))
                m)]
    (doseq [i (reverse (range n))]
      (send hd relay i))
    (.take q)))


(defn relay% [x i]
  (when (:next x)
    (send-off (:next x) relay% i))
  (when (and (zero? i) (:report-queue x))
    (dosync
     (alter (:report-queue x) conj i)))
  x)

(defn run% [m n]
  (let [q  (ref '())
        hd (nth (iterate #(agent {:next %})
                         (agent {:report-queue q}))
                m)]
    (doseq [i (reverse (range n))]
      (send-off hd relay% i))
    (loop [] (when (empty? @q) (recur)))
    @q))