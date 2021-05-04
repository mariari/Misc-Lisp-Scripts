(ns compilers.interpreter-type
  (:require [clojure.spec.alpha :as s])
  (:use [misc-scripts.macros]))


(def binop {:Plus + :Minus - :Times * :Div quot})

(declare expression?)
(defadt statement t
  (Compound [(stm1 statement?) (stm2 statement?)])
  (Assign   [(id   string?)    (exp  expression?)])
  (Print    [(exps (s/coll-of expression?))]))

(defadt expression t
  (Id-exp   [(id string?)])
  (Num-exp  [(int number?)])
  (Op-exp   [(exp1 expression?) op (exp2 expression?)])
  (Eseq-exp [(stm statement?) (exp expression?)]))
