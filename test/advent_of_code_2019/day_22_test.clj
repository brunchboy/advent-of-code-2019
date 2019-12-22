(ns advent-of-code-2019.day-22-test
  (:require [advent-of-code-2019.day-22 :as sut]
            [clojure.test :as test]
            [clojure.repl :refer :all]))

(def ten-deck
  "The tiny deck used in the examples."
  (vec (range 10)))

(test/deftest cuts
  (test/is (= [3 4 5 6 7 8 9 0 1 2] (sut/cut ten-deck 3)))
  (test/is (= [6 7 8 9 0 1 2 3 4 5] (sut/cut ten-deck -4))))

(test/deftest skip-deal
  (test/is (= [0 7 4 1 8 5 2 9 6 3] (sut/deal-with-increment ten-deck 3))))

(test/deftest shuffling
  (test/is (= [0 7 4 1 8 5 2 9 6 3] (sut/shuffle-deck ten-deck "deal with increment 3")))

  (test/is (= [0 3 6 9 2 5 8 1 4 7] (sut/shuffle-deck ten-deck "deal with increment 7
deal into new stack
deal into new stack")))

  (test/is (= [3 0 7 4 1 8 5 2 9 6] (sut/shuffle-deck ten-deck "cut 6
deal with increment 7
deal into new stack")))

  (test/is (= [6 3 0 7 4 1 8 5 2 9] (sut/shuffle-deck ten-deck "deal with increment 7
deal with increment 9
cut -2")))

  (test/is (= [9 2 5 8 1 4 7 0 3 6] (sut/shuffle-deck ten-deck "deal into new stack
cut -2
deal with increment 7
cut 8
cut -4
deal with increment 7
cut 3
deal with increment 9
deal with increment 3
cut -1"))))

(test/deftest part-1
  (test/is (= 6129 (sut/part-1))))

;; Part 2

(def prime-deck
  "A still-small deck but with a prime number of cards for
  experimenting with formulae for part 2."
  (vec (range 11)))

(test/deftest cut-formula
  "Checks that my shortcut for calculating the results of cuts
  works."
  []
  (let [deck-size (count prime-deck)]
    (test/is (empty? (filter identity
                             (for [cut-spec (range (- deck-size) deck-size)
                                   pos      (range deck-size)]
                               (not= (sut/card-after-cut deck-size cut-spec pos)
                                     (nth (sut/cut prime-deck cut-spec) pos))))))))

(test/deftest reverse-formula
  "Checks that my shortcut for calculating the results of deck
  reversals works."
  []
  (let [deck-size (count prime-deck)]
    (test/is (empty? (filter identity
                             (for [pos (range deck-size)]
                               (not= (sut/card-after-reverse deck-size pos)
                                     (nth (sut/deal-into-new-stack prime-deck) pos))))))))

;; Abandoned for second approach.
#_(test/deftest deal-with-increment-formula
  "Checks that my shortcut for calculating the results of cuts
  works."
  []
  (let [deck-size (count prime-deck)]
    (test/is (empty? (vec (filter identity
                                  (for [increment (range 1 deck-size)
                                        pos       (range deck-size)]
                                    (let [failed? (not= (sut/card-after-deal-with-increment deck-size increment pos)
                                                        (nth (sut/deal-with-increment prime-deck increment) pos))]
                                      (when failed?
                                        (println)
                                        (println "failed for increment" increment "and pos" pos))
                                      failed?))))))))

(test/deftest part-2-shuffles
  (let [deck (sut/factory-deck-2 11)]
    (let [shuffled (sut/shuffle-deck-2 deck "deal into new stack")]
      (test/is (= [10 9 8 7 6 5 4 3 2 1 0]
                  (map #(sut/nth-card shuffled (biginteger %)) (range 11)))))
    (let [shuffled (sut/shuffle-deck-2 deck "cut 2")]
      (test/is (= [2 3 4 5 6 7 8 9 10 0 1]
                  (map #(sut/nth-card shuffled (biginteger %)) (range 11)))))
    (let [shuffled (sut/shuffle-deck-2 deck "cut -3")]
      (test/is (= [8 9 10 0 1 2 3 4 5 6 7]
                  (map #(sut/nth-card shuffled (biginteger %)) (range 11)))))))

;; And now for the moment of truth: try comparing results between part 1 and part 2.
(test/deftest parts-equivalent
  (test/is (= 3201
              (sut/nth-card (sut/shuffle-deck-2 (sut/factory-deck-2 10007) sut/shuffling-instructions)
                            (biginteger 2019)))))

(test/deftest part-2
  (test/is (= 71345377301237 (sut/part-2))))
