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

(def test-dfa {:S0 {:start true :result :accept :transitions {0 :S0 1 :S1}}
               :S1 {:result :reject :transitions {0 :S0 1 :S1}}})

(defn build-transition
  [[char state]]
  `([[~char . ?rem] ?out] (~state ?rem ?out)))

(defn my-matche
  [state]
  (let [trans-vecs (seq (:transitions state))]
    `(matche [input out]
             ([[] ~(:result state)])
            ; ~(build-transition (first trans-vecs))
             ~@(map build-transition trans-vecs))))

(defn write-state-function
  [state-name state]
  `(~state-name [input output] ~(my-matche state)))
 
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
