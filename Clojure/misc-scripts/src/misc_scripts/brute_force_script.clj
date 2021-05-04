#!/usr/bin/env lein exec

(ns misc-scripts.brute-force-script
  {:lang :core.typed}
  (:use [uncomplicate.fluokitten core jvm])
  (:use [clojure.java.shell :only [sh]])
  (:require [clojure.core.typed :as t]))


(t/ann char-range [Character Character -> (t/ASeq Character)])
(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(t/ann brange (t/ASeq Character))
(def brange (concat (char-range \A \Z)
                    (char-range \a \z)
                    (char-range \0 \9)))

(t/ann curry-str (t/IFn
                  [t/Any -> [t/Any * -> String]]
                  [t/Any t/Any * -> String]))
(defn curry-str
  ([c]         (partial curry-str c))
  ([c & chars] (apply str (cons c chars))))

(t/ann ^:no-check links (t/ASeq String))
(def links (map #(op "https://lainfile.pw/" %)
                (<*> (map curry-str brange) brange)))

(t/ann dwn-file [String -> nil])
(defn dwn-file [url]
  (println (:err (sh "wget" "--content-disposition" url))))

(t/ann ^:no-check main [-> nil])
(defn main []
  (run! dwn-file links))
