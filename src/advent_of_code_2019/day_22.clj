(ns advent-of-code-2019.day-22
  "Solutions to the Day 22 problems."
  (:require [clojure.math.combinatorics :as combo]
            [clojure.repl :refer :all]))

(defn cut-location
  "Determines the deck position at which a cut should be performed,
  transforming negative numbers into counting from the bottom of the
  deck."
  [deck-size where]
  (if (pos? where)
    where
    (+ deck-size where)))

(defn cut
  [deck where]
  (println "cut" where)
  (vec (apply concat (reverse (split-at (cut-location (count deck) where) deck)))))

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

(def shuffling-instructions
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
  (.indexOf (shuffle-deck factory-deck shuffling-instructions) 2019))

;; Part 2

(defn card-after-cut
  "Returns the position from which a card was taken to reach a specified
  position in the deck after the deck has been cut at the specified
  location. As with part 1, negative locations mean to count from the
  bottom."
  [deck-size cut-spec ending-position]
  (mod (+ cut-spec ending-position) deck-size))

(defn card-after-reverse
  "Returns the position from which a card was taken to reach a specified
  position in the deck after the deck has been reversed."
  [deck-size ending-position]
  (- deck-size ending-position 1))

;; This turns out only to work when the increment is smaller than about half
;; of the deck size. Time to break out more number theory...
(defn card-after-deal-with-increment
  "Returns the position from which a card was taken to reach a specified
  position in the deck after the deck has been shuffled by dealing
  with the specified increment."
  [deck-size increment ending-position]
  (let [cards-per-pass   (inc (quot deck-size increment))
        pass-number      (mod ending-position increment)
        card-within-pass (quot ending-position increment)]
    (+ (* pass-number cards-per-pass) card-within-pass)))

;; Part 2 take two

(def deck-size
  "The number of cards in the crazy big deck."
  (biginteger 119315717514047))

(def shuffle-count
  "The number of times we need to run our shuffle algorithm."
  (biginteger 101741582076661))

;; To simplify the math greatly, the result of any combination of our
;; shuffle steps can be described as a combination of the number of
;; cards in the deck, an offset (describing where the deck was cut,
;; with the factory deck being offset 0), and an increment (where the
;; factory deck has an increment of 1, so each card is one higher than
;; the preceding card).

(defn nth-card
  "This function returns the nth card from a deck described by the tuple
  of `num-cards`, `offset` and `increment`."
  [[num-cards offset increment] i]
  (.mod (.add offset (.multiply i increment)) num-cards))

(defn shuffle-step-2
  "Calculates the resulting offset and increment parameters for an input
  deck and a shuffling instruction."
  [[num-cards offset increment] instruction]
  (or
   (when (= instruction "deal into new stack") ; Reverse the deck:
     (let [new-increment (.mod (.negate increment) num-cards)]  ; Negate the increment but keep it in legal range.
       [num-cards (.mod (.add offset new-increment) num-cards) new-increment]))  ; Then rotate deck one card left.

   (when-let [matches (re-matches #"cut (-?\d+)" instruction)] ; Rotate the deck a number of cards to the left.
     (let [distance (biginteger (last matches))]
       [num-cards (.mod (.add offset (.multiply distance increment)) num-cards) increment]))

   (when-let [matches (re-matches #"deal with increment (\d+)" instruction)]  ; Compose increments.
     ;; The increment is multipled by the modular inverse of the new increment.
     (let [deal-increment (biginteger (last matches))]
       [num-cards offset (.mod (.multiply increment (.modInverse deal-increment num-cards)) num-cards)]))

   (println "Unrecognized instruction:" instruction)))

(defn shuffle-deck-2
  "Calculates the offset and increment parameters resulting from a set
  of shuffling instructions on a factory deck."
  [deck instructions]
  (reduce shuffle-step-2 deck (clojure.string/split-lines instructions)))

(defn factory-deck-2
  "Creates a factory deck for part 2. With no arguments uses the full
  problem deck size. For unit testing can supply a deck size."
  ([]
   (factory-deck-2 deck-size))
  ([size]
   [(biginteger size) (biginteger 0) (biginteger 1)]))

(defn repeat-shuffles
  "Calculates the results from repeating a particular shuffle multiple times."
  [[num-cards offset increment] iterations]
  (let [new-increment (.modPow increment iterations num-cards)
        base (.mod (.subtract BigInteger/ONE increment) num-cards)]
    [num-cards
     (.mod (.multiply offset (.multiply (.subtract BigInteger/ONE new-increment)
                                        (.modInverse base num-cards)))
           num-cards)
     new-increment]))

(defn part-2
  "Solve part 2"
  []
  (let [shuffled (shuffle-deck-2 (factory-deck-2) shuffling-instructions)]
    (nth-card (repeat-shuffles shuffled shuffle-count) (biginteger 2020))))
