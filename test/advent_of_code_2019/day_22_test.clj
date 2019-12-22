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

(test/deftest cycle
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
