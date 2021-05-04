(defn print-seq [s]
  (when (seq s)
    (prn (first s))
    (recur (rest s))))

(defn print-seq% [[a & b]]
  (when a (prn a) (recur b)))

(defn calculate []
  (reduce + (map #(* % %) (filter odd? (range 10)))))

(defn calculate% []
  (->> (range 10) (filter odd?) (map #(* % %)) (reduce +)))

(defmacro compose [& rest]
  `(fn [x#] (->> x# ~@(reverse rest))))

(def calculat
  (compose (reduce +) (map #(* % %)) (filter odd?)))


(def guys-name-map
  {:f-name "Guy" :m-name "Lewis" :l-name "Steele"})

(let [{f-name :f-name
       m-name :m-name
       l-name :l-name} guys-name-map]
  (str l-name ", " f-name " " m-name))

(let [{:keys [f-name m-name l-name]} guys-name-map]
  (str l-name ", " f-name " " m-name))


;; binds default of title to the or instead of nil
(let [{:keys [title f-name m-name l-name]
       :or {title "Mr."}} guys-name-map]
(println title f-name m-name l-name))


(let [{first-thing 0, last-thing 3} [1 2 3 4]]
  [first-thing last-thing])

;; for is kinda like haskell's list comprehension
(for [x (range 2) y (range 2)] [x y])

(find-doc "xor")

(for [x (range 2) y (range 2)]
  [x y (bit-xor x y)])


(defn xors [max-x max-y]
  (for [x (range max-x) y (range max-y)]
    [x y (bit-xor x y)]))


(xors 2 2)

;; (def frame (java.awt.Frame.))


;; (for [method (seq (.getMethods java.awt.Frame))
;;       :let [method-name (.getName method)]
;;       :when (re-find #"Vis" method-name)]
;;   method-name)

;; (.setVisible frame true)

;; (.setSize frame (java.awt.Dimension. 200 200))

;; gives us the javadoc
;; (javadoc frame)

;; (def gfx (.getGraphics frame))

;; (.setSize frame (java.awt.Dimension. 200 200))

;; (doseq [[x y xor] (xors 200 200)]
;;   (.setColor gfx (java.awt.Color. xor xor xor))
;;   (.fillRect gfx x y 1 1))


;; (.setVisible frame false)
