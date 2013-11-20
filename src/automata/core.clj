(ns automata.core
  (:gen-class)
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd]))

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

(defn mult-3-logico*
  [input curr-state]
  (run* [q]
        (conde
         [(== curr-state :S0)
          (conde
           [(== '() input) (== q 'accept)]
           [(== (first input) \0) (== q (mult-3-logico* (rest input) :S0))]
           [(== (first input) \1) (== q (mult-3-logico* (rest input) :S1))])]
         [(== curr-state :S1)
           (conde
           [(== '() input) (== q 'reject)]
           [(== (first input) \0) (== q (mult-3-logico* (rest input) :S2))]
           [(== (first input) \1) (== q (mult-3-logico* (rest input) :S0))])]
         [(== curr-state :S2)
          (conde
           [(== '() input) (== q 'reject)]
           [(== (first input) \0) (== q (mult-3-logico* (rest input) :S1))]
           [(== (first input) \1) (== q (mult-3-logico* (rest input) :S2))])])))

(defn mult-3-logico
  [input]
  (mult-3-logico* (seq (Integer/toString input)) :S0))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println (mult-3 1001)))
