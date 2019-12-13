(ns advent-of-code-2019.day-13-test
  "Unit tests for day 12."
  (:require [clojure.test :as test]
            [advent-of-code-2019.day-13 :as sut]))

(test/deftest part-1
  (test/is (= 251 (sut/part-1))))
