(ns automata.core
  (:gen-class
   (:use [clojure.core.logic])))

(defn build-matche-state-transition
  [[start-char transition-state]]
  `([[~start-char . ?rem] ?out] (~transition-state ?rem ?out)))

(defmacro build-all-matche-state-transitions
  [transitions]
  ())

(defmacro build-dfa
  [dfa]
  `(run 10 [clojure.core.logic/q] (== clojure.core.logic/q ~dfa)))

;; (map inc [..])
(defmacro test1
  [x]
  `(map inc ~x))

;; 
(defmacro test2
  [x]
  `(println ~x))

(defmacro test3
  []
  `(defn bar [] (println "hello")))

;; (map inc [..])
;; (println [..])
(defmacro test
  [x]
  `(test1 ~x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def test-dfa {'S0 {:start true :result :accept :transitions {0 'S0 1 'S1}}
               'S1 {:result :reject :transitions {0 'S0 1 'S1}}})

(defn build-transition
  [[char state]]
  `([[~char . ?rem#] ?out] (~state ?rem# ?out#)))

(defn my-matche
  [state]
  (let [trans-vecs (seq (:transitions state))]
    (cons `([[] ~(:result state)])
          `(~@(map build-transition trans-vecs)))))

(defn hash-symbol-name
  [sym-name]
  (symbol (str (str sym-name) "#")))

(defn write-state-function
  [[state-name state]]
  `(~(hash-symbol-name state-name) [input# out#]
                (matche [input# out#]
                        ~@(my-matche state))))

(defn get-start-state
  [dfa]
  (-> (filter (fn [[name state]] (-> state :start nil? not)) (seq dfa)) first first))

(defn write-run-expr
  [dfa]
  `(defn my-dfa [input#]
     (run 5 [q#]
           (letfn [~@(map write-state-function (seq dfa))]
             (~(hash-symbol-name (get-start-state dfa)) input# q#)))))

(defmacro build-automata-fn [dfa]
  `~(write-run-expr dfa))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(run 10 [q]
     (letfn ))

(defn get-vec
  []
  [1 2 3])

(defn hello
  []
  `(defn foo [] (println "hello world")))

(defmacro func []
  `~(hello))


;works
(defmacro test
  [dfa]
  `(defn my-dfa [input#]
     ~(p)))


;; works
(defmacro test
  [dfa]
  `(defn my-dfa [input#]
     (run 10 [q#] (== q# input#))))

;; works
(defmacro test
  []
  `(run 10 [q#]
       (matche [[\a \b \c] q#]
               ([[] :accept])
               ([[\a . ?rem#] :reject]))))
;; works
(defmacro test
  []
  `(defn basic [input#]
     (run 10 [q#]
          (conde
           [
            (matche [input# q#]
                    ([[] :accept])
                    ([[\a . ?rem#] :reject]))]
           [(== q# input#)]))))

(run 10 [q])

;; works
(defmacro test
  [val]
  `(defn pval [input#]
     (println input#)))

;works
(defmacro foo
  []
  `(defn bar [x#] (println "hello")))

(defn mrec
  [input]
  (run 10 [q]
       (letfn [(S0 [input]
                 (== q input))
               (S1 [input] (== q input))]
         (S0 input))))

(defmacro ltest
  []
  `(defn mrec
     [input#]
     (run 10 [q#]
          (letfn [(S0# [input#]
                    (== q# input#))
                  (S1# [input#] (== q# input#))]
            (S0# input#)))))

;; works
(defmacro ltest
  []
  `(run 10 [q#]
     (letfn [(S0# []
               (== q# 2))
             (S1# [] (== q# 1))]
       (S0#))))

(defn mult-3-logico
  [input]
  (run 10 [q] 
       (letfn [(S0
                 [input out]
                 (matche [input out]
                         ([[] 'accept])
                         ([[\0 . ?rem] ?out] (S0 ?rem ?out))
                         ([[\1 . ?rem] ?out] (S1 ?rem ?out))))
               (S1
                 [input out]
                 (matche [input out]
                         ([[] 'reject])
                         ([[\0 . ?rem] ?out] (S2 ?rem ?out))
                         ([[\1 . ?rem] ?out] (S0 ?rem ?out))))
               (S2
                 [input out]
                 (matche [input out]
                         ([[] 'reject])
                         ([[\0 . ?rem] ?out] (S1 ?rem ?out))
                         ([[\1 . ?rem] ?out] (S2 ?rem ?out))))]
         (S0 (seq (Integer/toString input)) q))))



(def
 automata.core/my-dfa
 (clojure.core/fn
  ([input__6975__auto__]
   (clojure.core.logic/run
    5
    [q__6972__auto__]
    (clojure.core/letfn
        [

      (S0
       [input__7150__auto__ out__7151__auto__]
       (clojure.core.logic/matche
        [input__7023__auto__ out__7024__auto__]
        ([[] :accept])
        ([[0 . ?rem__6997__auto__] ?out__6998__auto__]
         (S0 ?rem__6997__auto__ ?out__6998__auto__))
        ([[1 . ?rem__6997__auto__] ?out__6998__auto__]
          (S1 ?rem__6997__auto__ ?out__6998__auto__))))

      
      (S1
       [input__7150__auto__ out__7151__auto__]
       (clojure.core.logic/matche
        [input__7023__auto__ out__7024__auto__]
        ([[] :reject])
        ([[0 . ?rem__6997__auto__] ?out__6998__auto__]
         (S0 ?rem__6997__auto__ ?out__6998__auto__))
        ([[1 . ?rem__6997__auto__] ?out__6998__auto__]
         (S1 ?rem__6997__auto__ ?out__6998__auto__))))]
     (S0 input__6973__auto__ q__6972__auto__))))))
