(ns compilers.interpreter
  (:require [clojure.spec.alpha :as s]
            [lambdaisland.uniontypes :refer [case-of]]
            [clojure.spec.gen.alpha :as gen])
  (:use [misc-scripts.macros]
        [compilers.interpreter-type]
        [compilers.parser]))

;; (def prog% (run-statement-file "./src/misc_scripts/fun/compilers/test.txt"))
;; these are the same!
(def prog (->Compound (->Assign "a" (->Op-exp (->Num-exp 5) :Plus (->Num-exp 3)))
                      (->Compound (->Assign "b" (->Eseq-exp (->Print [(->Id-exp "a")
                                                                      (->Op-exp (->Id-exp "a") :Minus (->Num-exp 1))])
                                                            (->Op-exp (->Num-exp 10) :Times (->Id-exp "a"))))
                                  (->Print [(->Id-exp "b")]))))

(declare max-args-exp)
(defn max-args [stm]
  (case-of :t/statement stm
    :t/Compound {s1 :stm1 s2 :stm2} (max (max-args s1) (max-args s2))
    :t/Assign   {exp :exp}          (max-args-exp exp)
    :t/Print    {exps :exps}        (apply max (count exps) (map max-args-exp exps))))


(defn max-args-exp [exp]
  (cond (Eseq-exp? exp) (max-args (:stm exp))
        :else            0))

(declare interp-stm interp-exp)
(defn interp [stm]
  (interp-stm stm {}))

(defn interp-stm [stm state]
  (case-of :t/statement stm
    :t/Compound {s1 :stm1 s2 :stm2} (->> state (interp-stm s1) (interp-stm s2))
    :t/Assign   {id :id  e :exp}    (let [[val state] (interp-exp e state)]
                                      (assoc state id val))
    :t/Print    {exps :exps}        (letfn [(f [print-stm? state exp]
                                              (let [[val new-state] (interp-exp exp state)]
                                                (print val)
                                                (print-stm? " ")
                                                new-state))]
                                      (let [final-state (f identity
                                                           (reduce (partial f print) state (butlast exps))
                                                           (last exps))]
                                        (println "")
                                        final-state))))

(defn interp-exp [exp state]
  (case-of :t/expression exp
    :t/Id-exp   {var :id}       [(state var) state]
    :t/Num-exp  {int :int}      [int state]
    :t/Eseq-exp {s :stm e :exp} (let [[val _] (->> state (interp-stm s) (interp-exp e))]
                                  [val state])
    :t/Op-exp {e1 :exp1 op :op e2 :exp2}
    (let [[val1 state']  (interp-exp e1 state)
          [val2 state''] (interp-exp e2 state')]
      [((op binop) val1 val2) state''])))
