(ns misc-scripts.macros
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as st])
  (:use [uncomplicate.fluokitten core jvm]
        [lambdaisland.uniontypes :refer [case-of]]
        (misc-scripts.functions)))

(require 'clojure.core)

;; compose that already applies partial to all the arguments
(defmacro compose [& rest]
  "A macro version of compose that properly curries functions"
  `(fn [x#] (->> x# ~@(reverse rest))))

;; http://clj-me.cgrand.net/2011/06/17/a-flatter-cond/
;; I would probably just delay my lets instead
(defmacro cond-f
  "A variation on cond which sports let bindings:
     (cond-f
       (odd? a) 1
       :let [a (quot a 2)]
       (odd? a) 2
       :else 3)"
  [& clauses]
  (when-let [[test expr & clauses] (seq clauses)]
    (if (= :let test)
      `(let ~expr (cond-f ~@clauses))
      `(if ~test ~expr (cond-f ~@clauses)))))


(defmacro defadt [name name-space & body]
  (letfn [(first?       [x]              (if (list? x) (first x) x))
          (create-colon [name]           (keyword (str name-space) (str name))) ; (ns-name *ns*)
          (create-?     [name]           (symbol  (str name "?")))
          (create-pred  [name question] `(defn ~(create-? name) [val#] (~question ~name val#)))]
    (let [type-name              (create-colon name)
          cases                  (take-while list? body)
          options-and-interfaces (drop-while list? body)]
      `(do (defn ~(create-? name) [val#] (s/valid? ~type-name val#))
           ~@(map (fn [[name fields]] `(defrecord ~name ~(mapv first? fields) ~@options-and-interfaces)) cases)
           ~@(map (fn [[name _]]       (create-pred name instance?)) cases)
           ~@(mapcat (fn [[_ fields]]
                       (map (fn [[name type]] `(s/def ~(create-colon name) ~type)) (filter list? fields)))
                     cases)
           (s/def ~type-name (s/or ~@(mapcat (fn [[name fields]]
                                               [(create-colon name)
                                                `(s/and (s/keys :req-un ~(mapv create-colon (map first? fields)))
                                                        ~(create-? name))])
                                             cases)))))))


;; these two adt macros will replace all the defrecord and s/def entirely, once I put some time into it!
;; (import java.net.FileNameMap)
;; (declare expression?)
;; (defadt statement t
;;   (Compound [(stm1 statement?) (stm2 statement?)])
;;   (Assign   [(id   string?)    (exp expression?)])
;;   (Print    [(exps (s/coll-of expression?))])
;;   FileNameMap
;;   (getContentTypeFor
;;    [this fileName]
;;    (case-of :t/statement this
;;      :t/Compound {} (print "Compound")
;;      :t/Assign   {} (print "Assign")
;;      :t/Print    {} (print "Print"))))


;; (defadt expression t
;;   (Id-exp   [(id string?)])
;;   (Num-exp  [(int number?)])
;;   (Op-exp   [(exp1 expression?) op (exp2 expression?)])
;;   (Eseq-exp [(stm statement?) (exp expression?)]))
