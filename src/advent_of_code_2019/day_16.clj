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
  "Solves part 1."
  [input phases]
  (loop [digits (split-digits input)
         phases phases]
    (if (pos? phases)
      (recur (fft-phase digits) (dec phases))
      (clojure.string/join digits))))

;; Part 2

(defn fft2-phase
  "A slightly more efficient version of fft, based on noticing some
  patterns about which digits matter, but I hate this, it still took
  about two hours to find the solution."
  [digits]
  (let [sums (loop [result    []
                    sum       0
                    remaining digits]
               (if-let [current (first remaining)]
                 (recur (conj result sum)
                        (+ sum current)
                        (rest remaining))
                 result))]
    (loop [result []
           i      0]
      (if (< i (count digits))
        (recur
         (conj result (loop [element 0
                             sign    1
                             offsets (range i (count digits) (* 2 (inc i)))]
                        (if-let [offset (first offsets)]
                          (let [element (+ element (* sign (- (nth sums (min (+ i offset 1) (dec (count sums))))
                                                              (nth sums offset))))]
                     (recur element (- sign) (rest offsets)))
                          (mod (Math/abs element) 10))))
         (inc i))
        result))))

(defn fft2
  "Solve part 2... oh so slowly."
  [input phases]
  (loop [digits (vec (apply concat (repeat 10000 (split-digits input))))
         phases phases]
    (if (pos? phases)
      (do
        (println "phases:" phases)
        (recur (fft2-phase digits) (dec phases)))
      (let [offset (Long/valueOf (subs input 0 7))]
        (clojure.string/join (take 8 (drop offset digits)))))))
