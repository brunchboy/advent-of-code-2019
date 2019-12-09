(ns advent-of-code-2019.day-8-test
  "Unit tests for day 8."
  (:require [clojure.test :as test]
            [advent-of-code-2019.day-8 :as sut]))

(test/deftest flattening-works
  (test/is (= (sut/flatten-image (map #(Long/valueOf (str %)) "0222112222120000") 2 2)
              [" " "@" "@" " "])))
