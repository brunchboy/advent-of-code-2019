(ns advent-of-code.day-7-test
  "Unit tests for day 7."
  (:require [clojure.test :as test]
            [advent-of-code-2019.day-7 :as sut]))

(test/deftest example-program-1
  (test/is (= [[4 3 2 1 0] 43210]
              (sut/find-optimal-settings [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]))))

(test/deftest example-program-2
  (test/is (= [[0 1 2 3 4] 54321]
              (sut/find-optimal-settings [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]))))

(test/deftest example-program-3
  (test/is (= [[1 0 4 3 2] 65210]
              (sut/find-optimal-settings [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
                                          1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]))))

(test/deftest answer-1
  (test/is (= [[3 0 4 2 1] 30940]
              (sut/find-optimal-settings))))

;; Part 2

(test/deftest async-example-program-1
  (test/is (= [[9,8,7,6,5] 139629729]
              (sut/async-find-optimal-settings [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,
                                                1001,28,-1,28,1005,28,6,99,0,0,5]))))

(test/deftest async-example-program-2
  (test/is (= [[9,7,8,5,6] 18216]
              (sut/async-find-optimal-settings [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
                                                -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
                                                53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]))))

(test/deftest answer-2
  (test/is (= [[8 9 6 7 5] 76211147]
              (sut/async-find-optimal-settings))))
