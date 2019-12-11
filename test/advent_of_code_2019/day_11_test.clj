(ns advent-of-code-2019.day-11-test
  "Unit tests for day 11."
  (:require [clojure.test :as test]
            [advent-of-code-2019.day-11 :as sut]))

(test/deftest rotate
  (test/is (= :north (sut/turn :east 0)))
  (test/is (= :south (sut/turn :east 1)))
  (test/is (= :east (sut/turn :south 0)))
  (test/is (= :west (sut/turn :south 1))))

(test/deftest move
  (test/is (= [1 0] (sut/move [1 1] :north)))
  (test/is (= [2 1] (sut/move [1 1] :east)))
  (test/is (= [0 1] (sut/move [1 1] :west)))
  (test/is (= [1 2] (sut/move [1 1] :south))))

(test/deftest part-1
  (test/is (= 1909 (count (keys (sut/paint-panels))))))
