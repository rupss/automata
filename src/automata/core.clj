(ns automata.core
  (:gen-class))

(defn mult-3*
  [input curr-state]
  (let [ch (first input)]
    (cond
     (= curr-state :S0) ((fn [x] (cond
                                   (nil? x) 'accept
                                   (= x \0) (mult-3* (rest input) :S0)
                                   (= x \1) (mult-3* (rest input) :S1))) ch)
     (= curr-state :S1) ((fn [x] (cond
                                   (nil? x) 'reject
                                   (= x \0) (mult-3* (rest input) :S2)
                                   (= x \1) (mult-3* (rest input) :S0))) ch)
     (= curr-state :S2) ((fn [x] (cond
                                  (nil? x) 'reject
                                  (= x \0) (mult-3* (rest input) :S1)
                                  (= x \1) (mult-3* (rest input) :S2))) ch))))

(defn mult-3
  [input]
  (mult-3* (seq (Integer/toString input)) :S0))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
