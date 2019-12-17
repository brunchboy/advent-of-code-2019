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

;; Part 2, a much better approach discovered Sarah Murphy, based on
;; noticing that in addition to only needing to deal with the end of
;; the digits and work backwards, the coefficients follow Pascal's
;; triangle.
(defn generate-phases
  "Create a lazy sequence "
  [input]
  (iterate (partial reductions #(mod (+ %1 %2) 10)) input))

(defn fft2
  "Solves part 2, much faster!"
  [input phases]
  (let [offset (Long/valueOf (subs input 0 7))
        digits (vec (drop offset (apply concat (repeat 10000 (split-digits input)))))]
    (apply str (map #(mod % 10)
                    (take 8 ;; take the first 8, since a bunch of numbers were dropped above
                          (reverse
                           (nth (generate-phases (vec (reverse digits))) phases)))))))
