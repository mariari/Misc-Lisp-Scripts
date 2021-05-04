(require '[clojure.core.match :refer [match] :as m]
         '[defun.core :refer [defun] :as d]
         ;; '[clojure.set]
         '[clojure.tools.trace :as c]
         '[clojure.algo.monads :as mo]
         )

;; Use does not give it a name
(use 'clojure.set)
;; (use 'clojure.tools.trace)

;; (doseq [n (range 1 101)]
;;   (println
;;     (match [(mod n 3) (mod n 5)]
;;       [0 0] "FizzBuzz"
;;       [0 _] "Fizz"
;;       [_ 0] "Buzz"
;;       :else n)))
(def ^:dynamic *x* 1)

(defun fact-tco
  ([0 acc] acc)
  ([n acc] (recur (dec n) (*' acc n)))
  ([n] (recur n 1)))


(defn fact-tco%
  ([n] (fact-tco% n 1))
  ([n acc]
   (if (= 0 n)
     acc
     (recur (dec n) (*' acc n)))))

(defn fact-tco%%
  ([n] (fact-tco% n 1))
  ([n acc]
   (match n
      [0]   acc
      :else (recur (dec n) (*' acc n)))))
;; when doing TCO instead of using revers, use a vector instead, as that is Order 1 insert

(defn fact-proper [n]
  "test"
  (apply *' (range 1 (inc n))))

(defun fact-cps
  "test"
  ([n]     (recur n identity))
  ([0 cps] (trampoline cps 1))
  ([n cps] (recur (dec n) (fn [v] #(cps (*' n v))))))


(defun fact-cps-comp
  ([0 cps] (cps 1))
  ([n cps] (recur (dec n) (comp cps #(*' n %))))
  ([n]     (recur n identity)))

(defn fac-cps
  ([n]     (fac-cps n identity))
  ([n cps] (if (zero? n)
             (cps 1)
             (recur (dec n) (comp cps #(*' n %))))))

(defn fib-cps
  ([n]     (fib-cps n identity))
  ([n cps] (cond (> 0 n)   (trampoline cps 0)
                 (zero? n) (trampoline cps 1)
                 :else     (recur (dec n) (fn [x] (fn [] (fib-cps (- n 2) (fn [y] (fn [] (cps (+ x y)))))))))))
(defn fib-cps%%
  ([n]     (fib-cps%% n identity))
  ([n cps] (letfn [(rec [n cps]
                     (cond (> 0 n)   (trampoline cps 0)
                           (zero? n) (trampoline cps 1)
                           :else     (recur (dec n) (fn [y] (cloj n cps y)))))
                   (cloj [n cps y]
                     (fn [] (rec (- n 2) (fn [x] #(cps (+ y x))))))]
             (trampoline rec n cps))))

;; WORKS properly-------------------------------------------------------------------------------------------------
(defn sum-cps
  ([xs]     (sum-cps xs identity))
  ([xs cps] (if (empty? xs)
              (trampoline cps 0)
              (recur (rest xs) (fn [x] #(cps (+ (first xs) x)))))))

(defn fib-cps%
  ([n]     (fib-cps n identity))
  ([n cps] (cond (> 0 n)   (cps 0)
                 (zero? n) (cps 1)
                 :else     (recur (dec n) #(fib-cps% (- n 2) (comp cps (partial + %)))))))

 (defn add-list [lis]
   (apply + lis))


(let [[a & [b] :as seq] '(1 2 3)] seq)

(defn add-seq-non
  ([seq] (add-seq-non seq 0))
  ([seq acc]
   (if (empty? (first seq))
     acc
     (recur (rest seq) (+ acc (first seq))))))

(defn add-seq-let
  ([seq]
   (add-seq-let seq 0))

  ([[a & b] acc]
   (if (nil? a)
     acc
     (recur b (+ acc a)))))

(defn add-seq-let-cps
  ([seq]
   (add-seq-let-cps seq identity))
  
  ([[a & b] cps]
   (if (nil? a)
     (cps a)
     (recur b (comp cps #(+ a %))))))

(defn add-seq-match
  ([seq]     (add-seq-match seq 0))
  ([seq acc] (match seq 
                    ([a & b] :seq) (recur b (+ a acc))
                    ([]      :seq) acc)))

(defun add-seq-defun
  ([seq]                (recur seq 0))
  ([([a & b] :seq) acc] (recur b (+ a acc)))
  ([([]      :seq) acc] acc))


(match ['(3 4)] 
       [([a :guard number? b] :seq)] (list a b)
       :else 't)

;; (defn eval-expr% [expr env]
;;   (match [expr]
;;          [(n :guard number?)]
;;          n
         
;;          [(x :guard symbol?)]
;;          (env x)
         
;;          [(('lambda [x] body) :seq)]
;;          (fn [arg]
;;            (eval-expr body (fn [y]
;;                              (if (= x y)
;;                                arg
;;                                (env y)))))
         
;;          [([rator rand] :seq)]
;;          ((eval-expr rator env)
;;           (eval-expr rand  env))))



(defn eval-expr [expr env]
  (match [expr]
         [(n :guard number?)]
         n
         
         [(['inc x] :seq)]
         (inc (eval-expr x env))

         [(['* x y] :seq)]
         (* (eval-expr x env)
            (eval-expr y env))

         [(['dec x] :seq)]
         (dec (eval-expr x env))

         [(['zero? x] :seq)]
         (zero? (eval-expr x env))

         [(['if t then else] :seq)]
         (if (eval-expr t env)
           (eval-expr then env)
           (eval-expr else env))
         
         [(x :guard symbol?)]
         (env x)
         
         [(('lambda [x] body) :seq)]
         (fn [arg]
           (eval-expr body (fn [y]
                             (if (= x y)
                               arg
                               (env y)))))
         
         [([rator rand] :seq)]
         ((eval-expr rator env)
          (eval-expr rand  env))))

;; '(eval-expr '(inc (inc 3)))

(eval-expr '(inc (inc 3)) (fn [y] (error-mode "lookup unbund")))

(eval-expr '(inc (((lambda [x] x) (lambda [y] y)) 5)) (fn [y] (error-mode "lookup unbund")))
(eval-expr '(((lambda [!]               
                      (lambda [n]
                              ((! !) n)))
              (lambda [!]
                      (lambda [n] 
                              (if (zero? n)
                                1
                                (* n ((! !) (dec n)))))))
             5)
           identity)

;; (cond (and (or (seq? seq) (sequential? seq)) (seq seq))
;;       (let [a (first seq) b (rest seq)]
;;           (recur b (+ a acc)))
;;       :else (cond
;;               (empty? seq) acc
;;               :else (throw (java.lang.IllegalArgumentException.
;;                             (str
;;                              "No matching clause: "
;;                              seq)))))
