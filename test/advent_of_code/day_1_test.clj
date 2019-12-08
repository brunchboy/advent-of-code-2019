(ns advent-of-code.day-1-test
  "Unit tests for day 1."
  (:require [clojure.test :as test]
            [advent-of-code-2019.day-1 :as sut]))

(test/deftest sample-1
  (test/is (= 2 (sut/fuel 12))))

(test/deftest sample-2
  (test/is (= 2 (sut/fuel 14))))

(test/deftest sample-3
  (test/is (= 654 (sut/fuel 1969))))

(test/deftest sample-4
  (test/is (= 33583 (sut/fuel 100756))))

(test/deftest answer-1
  (test/is (= 3262358 (sut/answer))))

;; Part 2

(test/deftest sample-5
  (test/is (= 2 (sut/fuel-recursive 12))))

(test/deftest sample-6
  (test/is (= 966 (sut/fuel-recursive 1969))))

(test/deftest sample-7
  (test/is (= 50346 (sut/fuel-recursive 100756))))

(test/deftest answer-2
  (test/is (= 4890696 (sut/answer-2))))
