
(ns joy.udp
  (:refer-clojure :exclude [get]))


(defn beget [o p]
  (assoc o ::prototype p))

(beget (:sub 0) {:super 1})

(def put assoc)

(defn get [m k]
  (when m
    (if-let [[_ v] (find m k)]
      v
      (recur (::prototype m) k))))

(get (beget {:sub 0} {:super 1}) :super)
