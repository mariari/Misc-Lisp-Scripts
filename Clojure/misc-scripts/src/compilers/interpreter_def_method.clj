;; using i because structs pretty print copies the name space before it
(ns i
  (:use [misc-scripts.functions]))

(require '[misc-scripts.functions])
;; (require '[user])
;; misc-scripts.compilers.interpreter, true name of namespace
;; sadly cats doesn't have the state monad, so we'll have to thread state through oursleves

;; we also have Binop = Plus | Minus | Times | Div
(def binop {:Plus + :Minus - :Times * :div quot})

;; data Stm
(defrecord Compound [stm1 stm2])
(defrecord Assign   [^String id   exp])
(defrecord Print    [exps])


;; data Exp
(defrecord Id-exp   [^String id])
(defrecord Num-exp  [int])
(defrecord Op-exp   [exp1 op exp2])
(defrecord Eseq-exp [stm exp])


(def prog (Compound. (Assign. "a" (Op-exp. (Num-exp. 5) :Plus (Num-exp. 3)))
                     (Compound. (Assign. "b" (Eseq-exp. (Print. [(Id-exp. "a")
                                                                 (Op-exp. (Id-exp. "a") :Minus (Num-exp. 1))])
                                                        (Op-exp. (Num-exp. 10) :Times (Id-exp. "a"))))
                                (Print. [(Id-exp. "b")]))))


(defn max-args-exp [exp]
  (cond (instance? Eseq-exp exp) (max-args (:stm exp))
        :else                    0))

(defmulti max-args (fn [stm] (gen-dispatch stm)))
(defmethod max-args Compound [{s1 :stm1 s2 :stm2}] (max (max-args s1) (max-args s2)))
(defmethod max-args Assign   [{exp :exp}]          (max-args-exp exp))
(defmethod max-args Print    [{exps :exps}]        (apply max (count exps) (map max-args-exp exps)))


(defn interp [stm]
  (interp-stm stm {}))

(defmulti interp-stm (fn [stm state] (gen-dispatch stm)))

(defmethod interp-stm Compound [{s1 :stm1 s2 :stm2} state]
  (->> state (interp-stm s1) (interp-stm s2)))

(defmethod interp-stm Assign [{id :id e :exp} state]
  (let [[val state] (interp-exp e state)]
    (assoc state id val)))

(defmethod interp-stm Print [{exps :exps} state]
  (letfn [(f [print-stm? state exp]
            (let [[val new-state] (interp-exp exp state)]
              (print val)
              (print-stm? " ")
              new-state))]
    (let [final-state (f identity
                         (reduce #(f print %1 %2) state (butlast exps))
                         (last exps))]
      (println "")
      final-state)))


(defmulti interp-exp (fn [exp state] (gen-dispatch exp)))

(defmethod interp-exp Id-exp   [{var :id}                  state] [(state var) state])
(defmethod interp-exp Num-exp  [{int :int}                 state] [int state])
(defmethod interp-exp Eseq-exp [{s :stm e :exp}            state] (->> state (interp-stm s) (interp-exp e)))
(defmethod interp-exp Op-exp   [{e1 :exp1 op :op e2 :exp2} state]
  (let [[val1 state']  (interp-exp e1 state)
        [val2 state''] (interp-exp e2 state')]
    (list ((op binop) val1 val2) state'')))