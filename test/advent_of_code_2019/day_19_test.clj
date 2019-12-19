(ns advent-of-code-2019.day-19-test
  (:require [advent-of-code-2019.day-19 :as sut]
            [clojure.test :as test]))

(test/deftest origin-covered
  (test/is (sut/beam-covers? 0 0)))

(test/deftest out-x-axis-not-covered
  (test/is (not (sut/beam-covers? 100 0))))

(test/deftest part-1
  (test/is (= 158 (sut/part-1))))

(test/deftest part-2
  (test/is (= [6191165] (sut/part-2))))
