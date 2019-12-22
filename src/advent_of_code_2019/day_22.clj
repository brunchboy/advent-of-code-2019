(ns advent-of-code-2019.day-22
  "Solutions to the Day 22 problems."
  (:require [clojure.math.combinatorics :as combo]
            [clojure.repl :refer :all]))

(defn cut
  [deck where]
  (println "cut" where)
  (if (pos? where)
    (vec (apply concat (reverse (split-at where deck))))
    (vec (apply concat (reverse (split-at (+ (count deck) where) deck))))))

(defn deal-with-increment
  [deck increment]
  (println "deal with increment" increment)
  (let [result (long-array (count deck))]
    (loop [i     0
           cards deck]
      (when-let [card (first cards)]
        (aset result i card)
        (recur (mod (+ i increment) (count deck))
               (rest cards))))
    (vec result)))

(defn deal-into-new-stack
  [deck]
  (println "deal into new stack")
  (vec (reverse deck)))

(defn shuffle-step
  [deck instruction]
  (or
   (when (= instruction "deal into new stack")
     (deal-into-new-stack deck))
   (when-let [matches (re-matches #"cut (-?\d+)" instruction)]
     (cut deck (Long/valueOf (last matches))))
   (when-let [matches (re-matches #"deal with increment (\d+)" instruction)]
     (deal-with-increment deck (Long/valueOf (last matches))))
   (do (println "Unrecognized instruction:" instruction)
       deck)))

(defn shuffle-deck
  [deck instructions]
  (reduce shuffle-step deck (clojure.string/split-lines instructions)))

(def part-1-instructions
  "deal into new stack
deal with increment 32
cut 5214
deal with increment 50
cut -7078
deal with increment 3
cut 5720
deal with increment 18
cut -6750
deal with increment 74
cut -6007
deal with increment 16
cut -3885
deal with increment 40
deal into new stack
cut -2142
deal with increment 25
deal into new stack
cut -1348
deal with increment 40
cut 3943
deal with increment 14
cut 7093
deal with increment 67
cut 1217
deal with increment 75
cut 597
deal with increment 60
cut -1078
deal with increment 68
cut -8345
deal with increment 25
cut 6856
deal into new stack
cut -4152
deal with increment 59
deal into new stack
cut -80
deal with increment 3
deal into new stack
deal with increment 44
cut 1498
deal with increment 18
cut -7149
deal with increment 58
deal into new stack
deal with increment 71
cut -323
deal into new stack
deal with increment 58
cut 1793
deal with increment 45
deal into new stack
cut 7187
deal with increment 48
cut 2664
deal into new stack
cut 8943
deal with increment 32
deal into new stack
deal with increment 62
cut -9436
deal with increment 67
deal into new stack
cut -1898
deal with increment 61
deal into new stack
deal with increment 14
cut 1287
deal with increment 8
cut 560
deal with increment 6
cut -2110
deal with increment 8
cut 9501
deal with increment 25
cut 4791
deal with increment 70
deal into new stack
deal with increment 5
cut 2320
deal with increment 47
cut -467
deal into new stack
deal with increment 19
cut -1920
deal with increment 16
cut -8920
deal with increment 65
cut -3986
deal with increment 3
cut -2690
deal with increment 35
cut -757
deal with increment 37
cut -1280
deal with increment 71
cut 3765
deal with increment 26
deal into new stack")

(def factory-deck
  (vec (range 10007)))

(defn part-1
  []
  (nth (shuffle-deck factory-deck part-1-instructions) 2019))
