(ns compilers.interpreter-spec
  (:require [clojure.spec.alpha :as s]
            [lambdaisland.uniontypes :refer [case-of]]
            [clojure.spec.gen.alpha :as gen]))

(def binop {:Plus + :Minus - :Times * :div quot})


;; data Stm
(defrecord Compound [stm1 stm2])
(defrecord Assign   [id   exp])
(defrecord Print    [exps])

;; data Exp
(defrecord Id-exp   [id])
(defrecord Num-exp  [int])
(defrecord Op-exp   [exp1 op exp2])
(defrecord Eseq-exp [stm exp])

;; this is a perfectly valid way of defining stm, however the arguments are untyped!
;; (s/def ::stm (s/or :Compound #(instance? Compound %)
;;                    :Assign   #(instance? Assign %)
;;                    :Print    #(instance? Print %)))
(s/def ::id   string?)
(s/def ::int  number?)
(s/def ::stm  #(s/valid? ::statement %))
(s/def ::stm1 ::stm)
(s/def ::stm2 ::stm)
(s/def ::exp  #(s/valid? ::expression %))
(s/def ::exp1 ::exp)
(s/def ::exp2 ::exp)
(s/def ::exps (s/coll-of #(s/valid? ::expression %)))

(s/def ::statement (s/or :Compound (s/keys :req-un [::stm1 ::stm2])
                         :Assign   (s/keys :req-un [::id ::exp])
                         :Print    (s/keys :req-un [::exps])))

(s/def ::expression (s/or :Id-exp   (s/keys :req-un [::id])
                          :Num-exp  (s/keys :req-un [::int])
                          :Op-exp   (s/keys :req-un [::exp1 ::op ::exp2])
                          :Eseq-exp (s/keys :req-un [::stm ::exp])))


(s/def ::test (s/or :Id-exp   (s/and (s/keys :req-un [::id]) #(instance? Id-exp %))
                    :Num-exp  (s/keys :req-un [::id])
                    :Eseq-exp (s/keys :req-un [::stm ::exp])))


(def prog (Compound. (Assign. "a" (Op-exp. (Num-exp. 5) :Plus (Num-exp. 3)))
                     (Compound. (Assign. "b" (Eseq-exp. (Print. [(Id-exp. "a")
                                                                 (Op-exp. (Id-exp. "a") :Minus (Num-exp. 1))])
                                                        (Op-exp. (Num-exp. 10) :Times (Id-exp. "a"))))
                                (Print. [(Id-exp. "b")]))))


(defn max-args [stm]
  (case-of ::statement stm
    :Compound {s1 :stm1 s2 :stm2} (max (max-args s1) (max-args s2))
    :Assign   {exp :exp}          (max-args-exp exp)
    :Print    {exps :exps}        (apply max (count exps) (map max-args-exp exps))))


(defn max-args-exp [exp]
  (cond (instance? Eseq-exp exp) (max-args (:stm exp))
        :else                    0))


(defn interp [stm]
  (interp-stm stm {}))

(defn interp-stm [stm state]
  (case-of ::statement stm
    :Compound {s1 :stm1 s2 :stm2} (->> state (interp-stm s1) (interp-stm s2))
    :Assign   {id :id  e :exp}    (let [[val state] (interp-exp e state)]
                                    (assoc state id val))
    :Print    {exps :exps}        (letfn [(f [print-stm? state exp]
                                            (let [[val new-state] (interp-exp exp state)]
                                              (print val)
                                              (print-stm? " ")
                                              new-state))]
                                    (let [final-state (f identity
                                                         (reduce #(f print %1 %2) state (butlast exps))
                                                         (last exps))]
                                      (println "")
                                      final-state))))

(defn interp-exp [exp state]
  (case-of ::expression exp
    :Id-exp   {var :id}       [(state var) state]
    :Num-exp  {int :int}      [int state]
    :Eseq-exp {s :stm e :exp} (->> state (interp-stm s) (interp-exp e))

    :Op-exp   {e1 :exp1 op :op e2 :exp2}
      (let [[val1 state']  (interp-exp e1 state)
            [val2 state''] (interp-exp e2 state')]
        [((op binop) val1 val2) state''])))
