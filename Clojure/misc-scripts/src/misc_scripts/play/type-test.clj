#!/usr/bin/env lein exec

(ns misc-scripts.type-test
  {:lang :core.typed}
  (:use [uncomplicate.fluokitten core jvm])
  (:use [clojure.java.shell :only [sh]])
  (:require [clojure.core.typed :as t]))


(t/ann char-range [(t/U Character Number)
                   (t/U Character Number) -> (t/ASeq Character)])
(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

;; [Chars]


;; [String]


;; String -> nil
(t/ann foo [Number -> Number])
(defn foo [b]
  (+ 2 b))


(foo 2)
(m)