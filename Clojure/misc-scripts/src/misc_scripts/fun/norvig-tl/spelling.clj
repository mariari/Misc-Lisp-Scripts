(require '[cats.core :as c])

(def alphabet  "abcdefghijklmnopqrstuvwxyz")

(defn words [text]
  (re-seq #"[a-z]+" (.toLowerCase text)))

(defn train [features]
  (reduce (fn [model x] (update model x #(if % (inc %) 1))) {} features))

(defn all-perms
  ([word f end]
   (all-perms word f end alphabet))
  ([word f end alpha]
   (c/alet [i (vec (range end)), c alpha]
           (str (subs word 0 i) c (subs word (f i))))))

;; these two are the same thing really!!
;; (c/<*> (map (fn [x] #(str (subs word 0 x) % (subs word (f x))))
;;             (range end))
;;        alpha)

(defn edits1 [word]
  (let [n         (count word)
        remove-1  (map #(str (subs word 0 %) (subs word (inc %))) (range n))
        replace-1 (all-perms word inc n)
        insert-1  (all-perms word identity (inc n))
        swap-1    (map #(str (subs word 0 %)
                             (nth word (inc %))
                             (nth word %)
                             (subs word (+ 2 %)))
                       (range (dec n)))]
    (distinct (concat remove-1 swap-1 replace-1 insert-1))))

(defn known [words nwords]
  (not-empty (set (filter nwords words))))

(defn known-edits2 [word nwords]
  (not-empty (set (filter nwords (mapcat edits1 (edits1 word))))))

(defn correct [word nwords]
  (let [candidates (or (known [word] nwords)
                      (known (edits1 word) nwords)
                      (known-edits2 word nwords)
                      [word])]
    (apply max-key #(nwords % 1) candidates)))

;; (def nwords (train (words (slurp "./fun/norvig-tl/big.txt"))))
;; (correct "misstake" nwords)