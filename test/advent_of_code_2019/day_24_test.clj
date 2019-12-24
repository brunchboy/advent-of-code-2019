(ns advent-of-code-2019.day-24-test
  (:require [advent-of-code-2019.day-24 :as sut]
            [clojure.test :as test]
            [clojure.repl :refer :all]))

(def sample-map
  "The final example map in the problem statement."
  ".....
.....
.....
#....
.#...")

(test/deftest read-sample-map
  "Makes sure we get the right biodiversity score for the example
  map."
  (test/is (= 2129920 (sut/read-map sample-map))))

(test/deftest find-bugs
  "We get the proper results when asking about bugs at coordinates."
  (let [eris (sut/read-map sample-map)]
    (test/is (= [[0 3] [1 4]]
                (filter (fn [[x y]] (sut/bug-present? eris [x y]))
                        (for [x (range 5)
                              y (range 5)]
                          [x y]))))))

(def initial-state
  "The sample initial state for showing how generations work."
  "....#
#..#.
#..##
..#..
#....")

(test/deftest neighbors
  "Makes sure the neighbors function works right."
  (test/is (= [[1 0] [0 1]] (sut/neighbors [0 0])))
  (test/is (= [[2 2] [4 2] [3 1] [3 3]] (sut/neighbors [3 2])))
  (test/is (= [[2 4] [4 4] [3 3]] (sut/neighbors [3 4]))))

(test/deftest count-occupied-neighbors
  "Makes sure we count adjacent bugs right."
  (let [eris (sut/read-map initial-state)]
    (test/is (= 1 (sut/count-occupied-neighbors eris [0 0])))
    (test/is (= 0 (sut/count-occupied-neighbors eris [1 0])))
    (test/is (= 2 (sut/count-occupied-neighbors eris [3 0])))
    (test/is (= 3 (sut/count-occupied-neighbors eris [4 1])))
    (test/is (= 1 (sut/count-occupied-neighbors eris [2 4])))))

(test/deftest generations
  "Make sure we get the same results for the sample initial state."
  (let [eris (sut/read-map initial-state)]

    (test/is (= (sut/read-map "#..#.
####.
###.#
##.##
.##..")
                (sut/generation eris)))

    (test/is (= (sut/read-map "#####
....#
....#
...#.
#.###")
                (nth (iterate sut/generation eris) 2)))

    (test/is (= (sut/read-map "#....
####.
...##
#.##.
.##.#
")
                (nth (iterate sut/generation eris) 3)))

    (test/is (= (sut/read-map "####.
....#
##..#
.....
##...")
                (nth (iterate sut/generation eris) 4)))))

(test/deftest part-1
  "Check solution of part 1."
  (test/is (= 32573535 (sut/part-1))))

(test/deftest count-bugs-in-grid
  "Check bug counting on single grid."
  (test/is (= 2 (sut/count-bugs-in-grid (sut/read-map sample-map))))
  (test/is (= 8 (sut/count-bugs-in-grid (sut/read-map initial-state)))))

(test/deftest count-bugs
  "Check bug counting on multi-level map."
  (test/is (= 12
              (sut/count-bugs {:levels {-1 (sut/read-map sample-map)
                                        0  (sut/read-map initial-state)
                                        1  (sut/read-map sample-map)}}))))

(test/deftest recursive-sample
  "Make sure we get the same result as the example solution for part
  2."
  (let [eris (sut/augment-map (sut/read-map initial-state))]
    (test/is (= 99 (sut/count-bugs (nth (iterate sut/generation-2 eris) 10))))))

(test/deftest part-2
  "Check the result for part 2."
  (test/is (= 1951 (sut/part-2))))
