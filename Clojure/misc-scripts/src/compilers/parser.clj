(ns compilers.parser
  (:require [blancas.kern.core]
            [blancas.kern.lexer :as t]
            [blancas.kern.lexer.java-style :as j])
  (:use [blancas.kern.core]
        [compilers.interpreter-type]))




(def language-def
  (assoc t/basic-def
         :reserved-names    ["print" ":=" "+" "/" "*" "-"]
         :identifier-start  letter
         :identifier-letter alpha-num
         :case-sensitive    true))

(def- rec (t/make-parsers language-def))

(def comma      (:comma      rec))
(def semi       (:semi       rec))
(def colon      (:colon      rec))
(def identifier (:identifier rec))
(def sym        (:sym        rec))
(def parens     (:parens     rec))
(def token      (:token      rec))
(def float-lit  (:float-lit  rec))
(def word       (:word       rec)) ; reserved words



(declare statement
         expression
         expression'
         binop-p)

(def inline-parser (>> white-space statement))

(def assign-stm
  (bind [var identifier
         _   (token ":=")
         exp expression]
    (return (->Assign var exp))))

(def print-stm
  (>> (word "print")
      (<$> ->Print (parens (sep-by comma expression)))))

(def id-exp  (<$> ->Id-exp identifier))
(def num-exp (<$> (comp ->Num-exp int) float-lit))
(def op-exp
  (bind [exp1  expression'
         binop binop-p
         exp2  expression]
    (return (->Op-exp exp1 binop exp2))))

(def eseq-exp
  (parens
   (bind [stm statement
          _   comma
          exp expression]
     (return (->Eseq-exp stm exp)))))

(def plus  (>> (token "+") (return :Plus)))
(def minus (>> (token "-") (return :Minus)))
(def mult  (>> (token "*") (return :Times)))
(def div   (>> (token "/") (return :Div)))


(def binop-p (<|> plus minus mult div))

(def expression (<|> (<:> op-exp) expression'))
(def expression' (<|> num-exp id-exp eseq-exp))


(def statement' (<|> print-stm assign-stm))
(def statement  (<$> (comp (partial reduce #(->Compound %2 %1)) reverse) (sep-end-by1 semi statement')))

(defn run-statement-file [file]
  (:value (parse-file statement file)))
