(ns advent-of-code-2019.day-18-test
  (:require [advent-of-code-2019.day-18 :as sut]
            [clojure.test :as test]
            [clojure.repl :refer :all]))

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
               :keys           {[1 1] "b", [7 1] "a"},
               :key-index      {"b" [1 1], "a" [7 1]}
               :doors          {[3 1] "a"},
               :steps          0,
               :pos            [5 1],
               :keys-found     #{},
               :doors-blocking #{},
               :visited        #{}
               :player        {[5 1] :player}}
              (sut/initial-state-for-map (sut/read-maze sample-maze-1)))))

(test/deftest add-route
  (let [state   (sut/initial-state-for-map (sut/read-maze sample-maze-1))
        updated (sut/add-route :start "a" (assoc state :steps 50 :doors-blocking #{"d" "f"} :keys-found #{"q"}))]
    (test/is (= {:routes
                 {:start
                  {"a" [50 #{{:keys-found #{"q"}, :doors-blocking #{"d" "f"}}}]}},
                 :doors          {[3 1] "a"},
                 :steps          50,
                 :doors-blocking #{"d" "f"},
                 :pos            [5 1],
                 :visited        #{},
                 :walls
                 #{[2 2] [0 0] [1 0] [7 2] [4 2] [3 0] [8 0] [5 2] [8 2] [8 1] [7 0]
                   [0 2] [2 0] [5 0] [6 2] [6 0] [1 2] [3 2] [0 1] [4 0]},
                 :keys           {[1 1] "b", [7 1] "a"},
                 :keys-found     #{"q"}
                 :key-index      {"b" [1 1], "a" [7 1]},
                 :player         {[5 1] :player}}
                updated))))

(test/deftest find-routes-1
  (let [state (sut/initial-state-for-map (sut/read-maze sample-maze-1))]
    (test/is (= {:routes         {:start {"a" [2 #{{:keys-found #{}, :doors-blocking #{}}}]}},
                 :doors          {[3 1] "a"},
                 :steps          2,
                 :doors-blocking #{},
                 :pos            [6 1],
                 :visited        #{[5 1] [6 1]},
                 :walls
                 #{[2 2] [0 0] [1 0] [7 2] [4 2] [3 0] [8 0] [5 2] [8 2] [8 1] [7 0]
                   [0 2] [2 0] [5 0] [6 2] [6 0] [1 2] [3 2] [0 1] [4 0]},
                 :keys           {[1 1] "b", [7 1] "a"},
                 :key-index      {"b" [1 1], "a" [7 1]}
                 :keys-found     #{}
                 :player         {[5 1] :player}}
                (sut/find-route :start "a" state)))
    (test/is (= {:routes         {"a" {"b" [6 #{{:keys-found #{}, :doors-blocking #{"a"}}}]}},
                 :doors          {[3 1] "a"},
                 :steps          6,
                 :doors-blocking #{"a"},
                 :pos            [2 1],
                 :visited        #{[7 1] [4 1] [5 1] [6 1] [3 1] [2 1]},
                 :walls
                 #{[2 2] [0 0] [1 0] [7 2] [4 2] [3 0] [8 0] [5 2] [8 2] [8 1] [7 0]
                   [0 2] [2 0] [5 0] [6 2] [6 0] [1 2] [3 2] [0 1] [4 0]},
                 :key-index      {"b" [1 1], "a" [7 1]},
                 :keys           {[1 1] "b", [7 1] "a"},
                 :keys-found     #{}
                 :player         {[5 1] :player}}
                (sut/find-route "a" "b" state)))))

(test/deftest solve-maze-1
  (test/is (= 8 (sut/solve (sut/read-maze sample-maze-1)))))

(def sample-maze-2
  "########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################
")

(test/deftest find-routes-2
  (let [state   (sut/initial-state-for-map (sut/read-maze sample-maze-2))
        state-2 (sut/find-route :start "a" state)
        state-3 (sut/find-route "a" "c" state-2)]
    (test/is (= {:routes         {:start {"a" [2 #{{:keys-found #{}, :doors-blocking #{}}}]}},
                 :doors          {[3 1] "d", [5 1] "e", [9 1] "c", [13 1] "a", [19 1] "b"},
                 :steps          2,
                 :doors-blocking #{},
                 :pos            [16 1],
                 :visited        #{[15 1] [16 1]},
                 :walls
                 #{[15 4] [11 2] [2 2] [0 0] [23 2] [23 0] [17 2] [1 0] [8 4] [7 2]
                   [18 0] [15 0] [7 4] [5 4] [18 2] [3 4] [11 0] [17 0] [12 2] [4 2]
                   [23 1] [13 2] [3 0] [9 0] [16 2] [18 4] [21 2] [13 0] [8 0] [5 2]
                   [17 4] [11 4] [1 4] [10 2] [23 4] [21 4] [12 0] [8 2] [16 0] [10 0]
                   [14 4] [12 4] [6 4] [21 0] [19 4] [0 3] [2 4] [20 0] [9 2] [10 4]
                   [7 0] [0 2] [2 0] [0 4] [19 0] [9 4] [14 2] [4 4] [5 0] [6 2] [13 4]
                   [6 0] [16 4] [1 2] [20 2] [23 3] [22 4] [3 2] [19 2] [20 4] [22 0]
                   [14 0] [0 1] [15 2] [4 0]},
                 :key-index
                 {"f" [1 1], "e" [7 1], "b" [11 1], "a" [17 1], "c" [21 1], "d" [1 3]},
                 :keys
                 {[1 1] "f", [7 1] "e", [11 1] "b", [17 1] "a", [21 1] "c", [1 3] "d"},
                 :keys-found     #{}
                 :player         {[15 1] :player}}
                state-2))
    (test/is (= {:routes
                 {:start {"a" [2 #{{:keys-found #{}, :doors-blocking #{}}}]},
                  "a"    {"c" [4 #{{:keys-found #{}, :doors-blocking #{"b"}}}]}},
                 :doors          {[3 1] "d", [5 1] "e", [9 1] "c", [13 1] "a", [19 1] "b"},
                 :steps          4,
                 :doors-blocking #{"b"},
                 :pos            [20 1],
                 :visited        #{[17 1] [19 1] [18 1] [20 1]},
                 :walls
                 #{[15 4] [11 2] [2 2] [0 0] [23 2] [23 0] [17 2] [1 0] [8 4] [7 2]
                   [18 0] [15 0] [7 4] [5 4] [18 2] [3 4] [11 0] [17 0] [12 2] [4 2]
                   [23 1] [13 2] [3 0] [9 0] [16 2] [18 4] [21 2] [13 0] [8 0] [5 2]
                   [17 4] [11 4] [1 4] [10 2] [23 4] [21 4] [12 0] [8 2] [16 0] [10 0]
                   [14 4] [12 4] [6 4] [21 0] [19 4] [0 3] [2 4] [20 0] [9 2] [10 4]
                   [7 0] [0 2] [2 0] [0 4] [19 0] [9 4] [14 2] [4 4] [5 0] [6 2] [13 4]
                   [6 0] [16 4] [1 2] [20 2] [23 3] [22 4] [3 2] [19 2] [20 4] [22 0]
                   [14 0] [0 1] [15 2] [4 0]},
                 :key-index
                 {"f" [1 1], "e" [7 1], "b" [11 1], "a" [17 1], "c" [21 1], "d" [1 3]},
                 :keys
                 {[1 1] "f", [7 1] "e", [11 1] "b", [17 1] "a", [21 1] "c", [1 3] "d"},
                 :keys-found     #{}
                 :player         {[15 1] :player}}
                state-3))
    (test/is (= {:start {"a" [2 #{{:keys-found #{}, :doors-blocking #{}}}]},
                 "a"          {"c" [4 #{{:keys-found #{}, :doors-blocking #{"b"}}}]},
                 "d"
                 {"f"
                  [44
                   #{{:keys-found     #{"e" "a" "b" "c"},
                      :doors-blocking #{"d" "e" "a" "b" "c"}}}]}}
                (:routes (sut/find-route "d" "f" state-3))))))

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
