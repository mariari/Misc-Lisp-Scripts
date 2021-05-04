(import java.util.HashMap)
(require '[clojure.core.match :refer [match] :as m]
         '[defun.core :refer [defun] :as d]
         '[clojure.tools.trace :as more])

;; Use does not give it ka name
(use 'clojure.set)

;; (require '[java.util.HashMap :as al])
(def x 42)

(.start (Thread. #(println "Answer: " x)))


(new java.util.HashMap {"foo" 42 "bar" 9 "baz" "quux"})


(.divide (java.math.BigDecimal. "42") 2M)

(let [origin (java.awt.Point. 0 0)]
  (set! (.x origin) 15)
  (str origin))

;;  equivalent to 
(.endsWith (.toString (java.util.Date.)) "17")


(.. (java.util.Date.) toString (endsWith "17"))

(-> (java.util.Date.) .toString (.endsWith "17"))

;; normal java way
(def props (java.util.HashMap.))
(.put props "HOME" "/home/me")
(.put props "SRC" "src")
(.put props "BIN" "classes")


(doto (HashMap.)
  (.put "HOME" "/home/me")
  (.put "SRC" "src")
  (.put "BIN" "classes"))

(hash-map "SRC" "src" 
          "BIN" "classes"
          "HOME" "/home/me")

(def blah {"SRC" "src", "BIN" "classes", "HOME" "/home/me"})

(defn hash-add [table [key data & more]]
  (if data
    (recur (conj {key data} table) more)
    table))

(hash-add {} '(1 as 2 bs 3 cs 9))          ; will ignore the 9 because it doesn't belong part of ka pair
