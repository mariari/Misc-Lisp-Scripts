(ns list
    (:use [misc-scripts.macros]))

(defadt my-list t
  (Cons [car (cdr my-list?)])
  (Empty))