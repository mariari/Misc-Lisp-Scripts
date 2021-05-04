(for [x [:a :b]
      y (range 5)
      :when (odd? y)]
  [x y])

(doseq [x [:a :b]
        y (range 5)
        :when (odd? y)]
  (prn x y))

;; (defn query [max]
;;   (SELECT [a b c]
;;           (FROM X
;;                 (LEFT-JOIN Y :ON (= X.a Y.b)))
;;           (WHERE (AND (< a 5) (< b ~max)))))


;; polymorphism
(defprotocol Concatenatable
  (cat [this other]))

(extend-type String
  Concatenatable
  (cat [this other]
    (.concat this other)))

(cat "House" "of gays")


(extend-type java.util.List
  Concatenatable
  (cat [this other]
    (concat this other)))

(cat [1 2 3] [4 5 6])
