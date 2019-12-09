(ns advent-of-code-2019.day-4-test
  "Unit tests for day 4."
  (:require [clojure.test :as test]
            [advent-of-code-2019.day-4 :as sut]))

(test/deftest part-1
  (test/is (sut/valid? "111111"))
  (test/is (not (sut/valid? "223450")))
  (test/is (not (sut/valid? "123789"))))

(test/deftest answer-1
  (test/is (= 925 (count (filter sut/valid? (range 271973 785962))))))

(test/deftest part-2
  (test/is (sut/valid-2? "112233"))
  (test/is (not (sut/valid-2? "123444")))
  (test/is (sut/valid-2? "111122")))

(test/deftest answer-2
  (test/is (= 607 (count (filter sut/valid-2? (range 271973 785962))))))
