(ns  misc-scripts.complex
  (:import  [java.lang Number]))

(defrecord Complex [^Number r ^Number i]
  Object
  (toString [{:keys [r i]}]
    (apply str
      (cond
        (zero? r) [(if (= i 1) "" i) "i"]
        (zero? i) [r]
        :else     [r (if (neg? i) "-" "+") i "i"]))))