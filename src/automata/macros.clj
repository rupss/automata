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
  [[char state] fn-names]
  `([[~char . ?rem#] ?out#] (~(fn-names state) ?rem# ?out#)))

(defn my-matche
  [state fn-names]
  (let [trans-vecs (seq (:transitions state))]
    (cons `([[] ~(:result state)])
          `(~@(map (fn [vec] (build-transition vec fn-names)) trans-vecs)))))

(defn write-state-function
  [[state-name state] fn-names]
  `(~(fn-names state-name) [input# out#]
                (matche [input# out#]
                        ~@(my-matche state fn-names))))

(defn get-start-state
  [dfa]
  (-> (filter (fn [[name state]] (-> state :start nil? not)) (seq dfa))
      first
      first))

(defn make-fn-name-map
  [dfa]
  (let [gensym-pairs (map #(hash-map % (gensym %)) (keys dfa))]
    (reduce conj {} gensym-pairs)))

(defn build-automata-fn
  [dfa]
  (let [fn-names (make-fn-name-map dfa)]
    (eval
     `(defn my-dfa [input#]
        (run 5 [q#]
             (letfn [~@(map #(write-state-function % fn-names) (seq dfa))]
               (~(fn-names (get-start-state dfa)) input# q#)))))))

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
       (letfn [(S0 [input out]
                 (matche [input out]
                         ([[] :accept])
                         ([[0 . ?rem] ?out] (S1 ?rem ?out))))
               (S1 [input out]
                 (matche [input out]
                         ([[] :reject])
                         ([[1 . ?rem] ?out] (S0 ?rem ?out))))]
         (S0 input q))))

(defn get-s0
  []
  '(S0))

;; works
(defn ltest
  [dfa]
  (println dfa)
  (let [fn-names {'S0 (gensym 'S0) 'S1 (gensym 'S1)}
        start-state (get-start-state dfa)
        gensym-start-state (fn-names start-state)
        ]
    (println start-state)
    (eval `(defn mrec
            [input#]
            (run 10 [q#]
                 (letfn [(~(fn-names 'S0) [input# out#]
                          (matche [input# out#]
                                  ~@(my-matche (test-dfa 'S0) fn-names)))
                         (~(fn-names 'S1) [input# out#]
                          (matche [input# out#]
                                  ~@(my-matche (test-dfa 'S1) fn-names)))]
                   (~gensym-start-state input# q#)))))))

(clojure.core/defn
 automata.core/mrec
 [input__10997__auto__]
 (clojure.core.logic/run
  10
  [q__10998__auto__]
  (clojure.core/letfn
   [(S011099
     [input__10997__auto__ out__10999__auto__]
     (clojure.core.logic/matche
      [input__10997__auto__ out__10999__auto__]
      ([[] :accept])
      ([[0 . ?rem__10389__auto__] ?out__10390__auto__]
       (S011099 ?rem__10389__auto__ ?out__10390__auto__))
      ([[1 . ?rem__10389__auto__] ?out__10390__auto__]
       (S011099 ?rem__10389__auto__ ?out__10390__auto__))))
    (S111100
     [input__10997__auto__ out__10999__auto__]
     (clojure.core.logic/matche
      [input__10997__auto__ out__10999__auto__]
      ([[] :accept])
      ([[0 . ?rem__10389__auto__] ?out__10390__auto__]
       (S011099 ?rem__10389__auto__ ?out__10390__auto__))
      ([[1 . ?rem__10389__auto__] ?out__10390__auto__]
       (S011099 ?rem__10389__auto__ ?out__10390__auto__))))])))

;; works
(defmacro ltest
  []
  (let [S0 (gensym 'S0)
        S1 (gensym 'S1)]
    `(defn mrec
       [input#]
       (run 10 [q#]
            (letfn [(~S0 [input# out#]
                      (matche [input# out#]
                              ([[] :accept])
                              ([[0 . ?rem#] ?out#] (~S1 ?rem# ?out#))
                              ([[1 . ?rem#] ?out#] (~S0 ?rem# ?out#))))
                    (~S1 [input# out#]
                      (matche [input# out#]
                              ([[] :reject])
                              ([[1 . ?rem#] ?out#] (~S0 ?rem# ?out#))))]
              (~S0 input# q#))))))

(def
 automata.core/mrec
 (clojure.core/fn
  ([input__10495__auto__]
   (clojure.core.logic/run
    10
    [q__10496__auto__]
    (clojure.core/letfn
     [(S010608
       [input__10495__auto__ out__10497__auto__]
       (clojure.core.logic/matche
        [input__10495__auto__ out__10497__auto__]
        ([[] :accept])
        ([[0 . ?rem__10498__auto__] ?out__10499__auto__]
         (S110609 ?rem__10498__auto__ ?out__10499__auto__))
        ([[1 . ?rem__10498__auto__] ?out__10499__auto__]
         (S010608 ?rem__10498__auto__ ?out__10499__auto__))))
      (S110609
       [input__10495__auto__ out__10497__auto__]
       (clojure.core.logic/matche
        [input__10495__auto__ out__10497__auto__]
        ([[] :reject])
        ([[1 . ?rem__10498__auto__] ?out__10499__auto__]
         (S010608 ?rem__10498__auto__ ?out__10499__auto__))))]
     (S010608 input__10495__auto__ q__10496__auto__))))))

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



;; WORKS
(clojure.core/defn
 automata.core/mrec
 [input__9070__auto__]
 (clojure.core.logic/run
  10
  [q__9071__auto__]
  (clojure.core/letfn
   [(S0__9072__auto__
     [input__9070__auto__ out__9073__auto__]
     (clojure.core.logic/matche
      [input__9070__auto__ out__9073__auto__]
      ([[] :accept])
      ([[0 . ?rem__9074__auto__] ?out__9075__auto__]
       (S1__9076__auto__ ?rem__9074__auto__ ?out__9075__auto__))))
    (S1__9076__auto__
     [input__9070__auto__ out__9073__auto__]
     (clojure.core.logic/matche
      [input__9070__auto__ out__9073__auto__]
      ([[] :reject])
      ([[1 . ?rem__9074__auto__] ?out__9075__auto__]
       (S0__9072__auto__ ?rem__9074__auto__ ?out__9075__auto__))))]
   (S0__9072__auto__ input__9070__auto__ q__9071__auto__))))
