(ns advent-of-code-2019.day-10-test
  "Unit tests for day 10."
  (:require [clojure.test :as test]
            [advent-of-code-2019.day-10 :as sut]))

(def sample-map-1
  "The first example asteroid map."
  ".#..#
.....
#####
....#
...##")

(test/deftest read-sample-1
  (test/is (= [[1 0] [4 0] [0 2] [1 2] [2 2] [3 2] [4 2] [4 3] [3 4] [4 4]]
              (sut/read-map sample-map-1))))

(test/deftest sample-1-angles-from-first
  (test/is (= [[4 0 3.141592653589793 3.0]
               [0 2 -1.1071487177940904 2.23606797749979]
               [1 2 -1.5707963267948966 2.0]
               [2 2 -2.0344439357957027 2.23606797749979]
               [3 2 -2.356194490192345 2.8284271247461903]
               [4 2 -2.5535900500422257 3.605551275463989]
               [4 3 -2.356194490192345 4.242640687119285]
               [3 4 -2.0344439357957027 4.47213595499958]
               [4 4 -2.214297435588181 5.0]]
              (sut/angles-from [1 0] (sut/read-map sample-map-1)))))

(test/deftest sample-1-angles-from-best
  (test/is (= [[4 0 3.141592653589793 3.0]
               [0 2 -1.1071487177940904 2.23606797749979]
               [1 2 -1.5707963267948966 2.0]
               [2 2 -2.0344439357957027 2.23606797749979]
               [3 2 -2.356194490192345 2.8284271247461903]
               [4 2 -2.5535900500422257 3.605551275463989]
               [4 3 -2.356194490192345 4.242640687119285]
               [3 4 -2.0344439357957027 4.47213595499958]
               [4 4 -2.214297435588181 5.0]]
              (sut/angles-from [1 0] (sut/read-map sample-map-1)))))

(test/deftest sample-1
  (test/is (= [8 [3 4]] (sut/find-best (sut/read-map sample-map-1)))))

(def sample-map-2
  "The second example asteroid map."
  "......#.#.
#..#.#....
..#######.
.#.#.###..
.#..#.....
..#....#.#
#..#....#.
.##.#..###
##...#..#.
.#....####")

(test/deftest sample-2
  (test/is (= [33 [5 8]] (sut/find-best (sut/read-map sample-map-2)))))

(def sample-map-3
  "The third example asteroid map."
  "#.#...#.#.
.###....#.
.#....#...
##.#.#.#.#
....#.#.#.
.##..###.#
..#...##..
..##....##
......#...
.####.###.")

(test/deftest sample-3
  (test/is (= [35 [1 2]] (sut/find-best (sut/read-map sample-map-3)))))

(def sample-map-4
  "The fourth example asteroid map."
  ".#..#..###
####.###.#
....###.#.
..###.##.#
##.##.#.#.
....###..#
..#.#..#.#
#..#.#.###
.##...##.#
.....#.#..")

(test/deftest sample-4
  (test/is (= [41 [6 3]] (sut/find-best (sut/read-map sample-map-4)))))

(def sample-map-5
  "The fifth example asteroid map."
  ".#..##.###...#######
##.############..##.
.#.######.########.#
.###.#######.####.#.
#####.##.#.##.###.##
..#####..#.#########
####################
#.####....###.#.#.##
##.#################
#####.##.###..####..
..######..##.#######
####.##.####...##..#
.#####..#.######.###
##...#.##########...
#.##########.#######
.####.#.###.###.#.##
....##.##.###..#####
.#.#.###########.###
#.#.#.#####.####.###
###.##.####.##.#..##")

(test/deftest sample-5
  (test/is (= [210 [11 13]] (sut/find-best (sut/read-map sample-map-5)))))

(test/deftest part-1
  (test/is (= [247 [20 21]] (sut/find-best (sut/read-map sut/part-1-map)))))

;; Part 2

(test/deftest normalize-angles
  (test/is (= [[5.81953769817878 4.47213595499958 1 0]
               [0.24497866312686423 4.123105625617661 4 0]
               [5.3003915839322575 3.605551275463989 0 2]
               [5.497787143782138 2.8284271247461903 1 2]
               [5.81953769817878 2.23606797749979 2 2]
               [0.0 2.0 3 2]
               [0.46364760900080615 2.23606797749979 4 2]
               [0.7853981633974483 1.4142135623730951 4 3]
               [1.5707963267948966 1.0 4 4]] (sut/normalized-angles-from [3 4] (sut/read-map sample-map-1)))))

;; The asteroids whose vaporization order is explained in prose right
;; before the problem statement:
(test/deftest vaporization-samples
  (let [order (sut/vaporization-order [11 13] (sut/read-map sample-map-5))]
    (test/is (= [11 12] (take 2 (drop 3 (first order)))))
    (test/is (= [12 1] (take 2 (drop 3 (nth order 1)))))
    (test/is (= [12 2] (take 2 (drop 3 (nth order 2)))))
    (test/is (= [12 8] (take 2 (drop 3 (nth order 9)))))
    (test/is (= [16 0] (take 2 (drop 3 (nth order 19)))))
    (test/is (= [16 9] (take 2 (drop 3 (nth order 49)))))
    (test/is (= [10 16] (take 2 (drop 3 (nth order 99)))))
    (test/is (= [9 6] (take 2 (drop 3 (nth order 198)))))
    (test/is (= [8 2] (take 2 (drop 3 (nth order 199)))))
    (test/is (= [10 9] (take 2 (drop 3 (nth order 200)))))
    (test/is (= [11 1] (take 2 (drop 3 (nth order 298)))))))

(test/deftest part-2
  (test/is (= [1 5.81953769817878 2.23606797749979 19 19]
              (nth (sut/vaporization-order [20 21] (sut/read-map sut/part-1-map)) 199))))
