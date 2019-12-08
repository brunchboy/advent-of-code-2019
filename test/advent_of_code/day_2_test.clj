(ns advent-of-code.day-2-test
  "Unit tests for day 2."
  (:require [clojure.test :as test]
            [advent-of-code-2019.day-2 :as sut]))


(test/deftest sample-1
  (test/is (= [3500,9,10,70,
               2,3,11,0,
               99,
               30,40,50]
              (sut/intcode [1,9,10,3,
                            2,3,11,0,
                            99,
                            30,40,50]))))

(test/deftest sample-2
  (test/is (= [2,0,0,0,99] (sut/intcode [1,0,0,0,99]))))

(test/deftest sample-3
  (test/is (= [2,3,0,6,99] (sut/intcode [2,3,0,3,99]))))

(test/deftest sample-4
  (test/is (= [2,4,4,5,99,9801] (sut/intcode [2,4,4,5,99,0]))))

(test/deftest sample-5
  (test/is (= [30,1,1,4,2,5,6,0,99] (sut/intcode [1,1,1,4,99,5,6,0,99]))))

(test/deftest answer-1
  (test/is (= 2782414 (first (sut/intcode)))))
