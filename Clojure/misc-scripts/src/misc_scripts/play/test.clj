#!/usr/bin/env lein exec

(ns misc_scripts.test
  {:lang :core.typed}
  (:use [uncomplicate.fluokitten core jvm])
  (:use [clojure.java.shell :only [sh]])
  (:require [clojure.core.typed :as t]))


(t/ann char-range [(t/U Character Number)
                   (t/U Character Number) -> (t/ASeq Character)])
(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))


(t/ann ^:no-check
       brute-range (t/ASeq Character))
(def brute-range (op (char-range \A \Z)
                     (char-range \a \z)
                     (char-range \0 \9)))


(t/ann curry-str (t/IFn
                  [t/Any -> [t/Any * -> String]]
                  [t/Any t/Any * -> String]))
(defn curry-str
  ([c]         (partial curry-str c))
  ([c & chars] (apply str (cons c chars))))

;; [Chars]
;; (char-range \A \C)

;; [String]


;; String -> nil
(t/ann foo [Number -> Number])
(defn foo [b]
  (+ 2 b))


(foo 2)
