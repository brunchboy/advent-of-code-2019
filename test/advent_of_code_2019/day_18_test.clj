(ns advent-of-code-2019.day-18-test
  (:require [advent-of-code-2019.day-18 :as sut]
            [clojure.test :as test]))

(def sample-maze-1
  "#########
#b.A.@.a#
#########")

(test/deftest read-maze-1
  (test/is (= {:walls
           {[2 2] :wall,
            [0 0] :wall,
            [1 0] :wall,
            [7 2] :wall,
            [4 2] :wall,
            [3 0] :wall,
            [8 0] :wall,
            [5 2] :wall,
            [8 2] :wall,
            [8 1] :wall,
            [7 0] :wall,
            [0 2] :wall,
            [2 0] :wall,
            [5 0] :wall,
            [6 2] :wall,
            [6 0] :wall,
            [1 2] :wall,
            [3 2] :wall,
            [0 1] :wall,
            [4 0] :wall},
           :keys {[1 1] "b", [7 1] "a"},
           :doors {[3 1] "a"},
           :player {[5 1] :player}}
              (sut/read-maze sample-maze-1))))

(test/deftest state-from-maze-1
  (test/is (= {:walls
               #{[2 2] [0 0] [1 0] [7 2] [4 2] [3 0] [8 0] [5 2] [8 2] [8 1] [7 0]
                 [0 2] [2 0] [5 0] [6 2] [6 0] [1 2] [3 2] [0 1] [4 0]},
               :keys       {[1 1] "b", [7 1] "a"},
               :doors      {[3 1] "a"},
               :visited    #{},
               :door-index {"a" [3 1]}
               :keys-found []}
              (sut/initial-state-for-map (sut/read-maze sample-maze-1)))))

(test/deftest solve-maze-1
  (test/is (= 8 (sut/solve (sut/read-maze sample-maze-1)))))

(def sample-maze-2
  "########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################
")

(test/deftest solve-maze-2
  (test/is (= 86 (sut/solve (sut/read-maze sample-maze-2)))))

(def sample-maze-3
  "########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################")

(test/deftest solve-maze-3
  (test/is (= 132 (sut/solve (sut/read-maze sample-maze-3)))))

(def sample-maze-4
  "#################
#i.G..c...e..H.p#
########.########
#j.A..b...f..D.o#
########@########
#k.E..a...g..B.n#
########.########
#l.F..d...h..C.m#
#################")

(test/deftest solve-maze-4
  (test/is (= 136 (sut/solve (sut/read-maze sample-maze-4)))))

(def sample-maze-5
  "########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################")

(test/deftest solve-maze-5
  (test/is (= 81 (sut/solve (sut/read-maze sample-maze-5)))))
