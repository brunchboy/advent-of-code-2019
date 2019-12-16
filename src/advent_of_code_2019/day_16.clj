(ns advent-of-code-2019.day-16
  "Solutions to the Day 16 problems."
  (:require [clojure.repl :refer :all]))

(defn build-fft-pattern
  "Creates the pattern list for multiplying elements in the flawed
  frequency transmission algorithm."
  [element]
  (drop 1 (apply concat (repeat (mapcat (partial repeat (inc element)) [0, 1, 0, -1])))))

(defn split-digits
  "Splits a number stored in a string into its digits as actual integers."
  [n]
  (map #(Character/digit % 10) n))

(defn fft-phase
  "Returns the result of applying fft to an input value."
  [digits]
  (let [sums (map (partial apply +) (for [element (range (count digits))
                                          :let    [pattern (build-fft-pattern element)]]
                                      (map * digits pattern)))]
    (map #(mod (Math/abs %) 10) sums)))

(defn fft
  [input phases]
  (loop [digits (split-digits input)
         phases phases]
    (if (pos? phases)
      (recur (fft-phase digits) (dec phases))
      (clojure.string/join digits))))

;; Part 2

(defn fft-element-phase
  "Returns a single element of the fft algorithm for an input value."
  [digits element]
  (let [sum (apply + (map * digits (build-fft-pattern element)))]
    (mod (Math/abs sum) 10)))

(defn fft-element
  "Calculate a single result element of the fft for a given number of phases."
  [input element phases]
  (loop [digits (split-digits input)
         phases phases]
    (if (pos? phases)
      (recur (fft-element-phase digits element) (dec phases))
      ;; No... this doesn't pan out, we still need to calulate all elements for input to the next phase.
      ;; I am sure there is some clever trick involving the fact we are doing things modulo 10, and/or
      ;; that the patterns are made up of just 0, 1, and -1... but this is outside my linear algebra reach.
      )))
