(ns advent-of-code-2019.day-12-test
  "Unit tests for day 12."
  (:require [clojure.test :as test]
            [advent-of-code-2019.day-12 :as sut]))

(def sample-1-initial-state
  "Initial state for the first example."
  [[-1 0 2 0 0 0]
   [2 -10 -7 0 0 0]
   [4 -8 8 0 0 0]
   [3 5 -1 0 0 0]])

(test/deftest sample-1-gravity-step
  (test/is (= [[-1 0 2 3 -1 -1]
               [2 -10 -7 1 3 3]
               [4 -8 8 -3 1 -3]
               [3 5 -1 -1 -3 1]]
              (sut/gravity-step sample-1-initial-state))))

(test/deftest sample-1-step-1
  (test/is (= [[2 -1 1 3 -1 -1]
               [3 -7 -4 1 3 3]
               [1 -7 5 -3 1 -3]
               [2 2 0 -1 -3 1]]
              (sut/step sample-1-initial-state))))

(test/deftest sample-1-step-10
  (test/is (= [[2 1 -3 -3 -2 1]
               [1 -8 0 -1 1 3]
               [3 -6 1 3 2 -3]
               [2 0 4 1 -1 -1]]
              (nth (iterate sut/step sample-1-initial-state) 10))))

(test/deftest sample-1-step-10-energy
  (test/is (= 179 (sut/energy (nth (iterate sut/step sample-1-initial-state) 10)))))

(test/deftest part-1
  (test/is (= 5350 (sut/energy (nth (iterate sut/step sut/initial-state) 1000)))))

;; Part 2

(test/deftest sample-1-steps-until-match
  (test/is (= 2772 (sut/steps-until-match sample-1-initial-state))))

(test/deftest part-2
  (test/is (= 467034091553512 (sut/steps-until-match sut/initial-state))))

;; Improved Part 2

(test/deftest sample-1-parallel-steps-until-match
  (test/is (= 2772 (sut/parallel-steps-until-match sample-1-initial-state))))

(test/deftest part-2-improved
  (test/is (= 467034091553512 (sut/parallel-steps-until-match sut/initial-state))))

;; Stateless Part 2

(test/deftest sample-1-stateless-steps-until-match
  (test/is (= 2772 (sut/stateless-steps-until-match sample-1-initial-state))))

(test/deftest part-2-stateless
  (test/is (= 467034091553512 (sut/stateless-steps-until-match sut/initial-state))))
