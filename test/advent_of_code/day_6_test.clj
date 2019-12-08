(ns advent-of-code.day-6-test
  "Unit tests for day 6."
  (:require [clojure.test :as test]
            [advent-of-code-2019.day-6 :as sut]))

(def sample-orbits
  "Simple system from examples."
  "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L")

(test/deftest count-sample-orbits
  (test/is (= 42 (sut/count-orbits (sut/read-orbits sample-orbits) 0 "COM"))))

(test/deftest count-orbits
  (test/is (= 254447 (sut/count-orbits))))

(test/deftest count-sample-orbits-better
  (test/is (= 42 (sut/count-orbits-better (sut/read-orbits-2 sample-orbits)))))

(test/deftest count-orbits-better
  (test/is (= 254447 (sut/count-orbits-better))))

;; Part 2

(def sample-orbits-2
  "Second simple system from examples."
  "COM)B
B)C
C)D
D)E
E)F
B)G
G)H
D)I
E)J
J)K
K)L
K)YOU
I)SAN")

(test/deftest sample-transfers
  (test/is (= 4 (sut/transfers-to-santa (sut/read-orbits-2 sample-orbits-2)))))

(test/deftest transfers-to-santa
  (test/is (= 445 (sut/transfers-to-santa))))
