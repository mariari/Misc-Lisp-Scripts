(ns misc-scripts.clojure
  (:use (misc-scripts.functions))
  (:use [uncomplicate.fluokitten core jvm]))

;; also called fapply
(<*> [+ -] (list 1 2 3 4 5 ))

;; <>
(op "stuff " "here")
