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
               :multi-pos      [[5 1]],
               :keys-found     [],
               :doors-blocking #{},
               :visited        #{}
               :player         {[5 1] :player}}
              (sut/initial-state-for-map (sut/read-maze sample-maze-1)))))

(test/deftest add-route
  (let [state   (sut/initial-state-for-map (sut/read-maze sample-maze-1))
        updated (sut/add-route :start "a" (assoc state :steps 50 :doors-blocking #{"d" "f"} :keys-found ["q"]))]
    (test/is (= {:routes
                 {#{:start "a"}
                  [50 #{{:keys-found ["q"], :doors-blocking #{"d" "f"}}}]},
                 :doors          {[3 1] "a"},
                 :steps          50,
                 :doors-blocking #{"d" "f"},
                 :pos            [5 1],
                 :multi-pos            [[5 1]],
                 :visited        #{},
                 :walls
                 #{[2 2] [0 0] [1 0] [7 2] [4 2] [3 0] [8 0] [5 2] [8 2] [8 1] [7 0]
                   [0 2] [2 0] [5 0] [6 2] [6 0] [1 2] [3 2] [0 1] [4 0]},
                 :keys           {[1 1] "b", [7 1] "a"},
                 :keys-found     ["q"]
                 :key-index      {"b" [1 1], "a" [7 1]},
                 :player         {[5 1] :player}}
                updated))))

(test/deftest find-routes-1
  (let [state (sut/initial-state-for-map (sut/read-maze sample-maze-1))]
    (test/is (= {:routes         {#{"a" "@"} [2 #{{:keys-found [], :doors-blocking #{}}}]}
                 :doors          {[3 1] "a"},
                 :steps          2,
                 :doors-blocking #{},
                 :pos            [6 1],
                 :multi-pos      [[5 1]],
                 :visited        #{[5 1] [6 1]},
                 :walls
                 #{[2 2] [0 0] [1 0] [7 2] [4 2] [3 0] [8 0] [5 2] [8 2] [8 1] [7 0]
                   [0 2] [2 0] [5 0] [6 2] [6 0] [1 2] [3 2] [0 1] [4 0]},
                 :keys           {[1 1] "b", [7 1] "a"},
                 :key-index      {"b" [1 1], "a" [7 1]}
                 :keys-found     []
                 :player         {[5 1] :player}}
                (sut/find-route #{"@" "a"} state)))
    (test/is (= {:routes         {#{"a" "b"} [6 #{{:keys-found [], :doors-blocking #{"a"}}}]}
                 :doors          {[3 1] "a"},
                 :steps          6,
                 :doors-blocking #{"a"},
                 :pos            [2 1],
                 :multi-pos      [[5 1]],
                 :visited        #{[7 1] [4 1] [5 1] [6 1] [3 1] [2 1]},
                 :walls
                 #{[2 2] [0 0] [1 0] [7 2] [4 2] [3 0] [8 0] [5 2] [8 2] [8 1] [7 0]
                   [0 2] [2 0] [5 0] [6 2] [6 0] [1 2] [3 2] [0 1] [4 0]},
                 :key-index      {"b" [1 1], "a" [7 1]},
                 :keys           {[1 1] "b", [7 1] "a"},
                 :keys-found     []
                 :player         {[5 1] :player}}
                (sut/find-route #{"a" "b"} state)))
    (test/is (= {#{"b" "@"} [4 #{{:keys-found [], :doors-blocking #{"a"}}}],
                 #{"a" "@"} [2 #{{:keys-found [], :doors-blocking #{}}}],
                 #{"a" "b"} [6 #{{:keys-found [], :doors-blocking #{"a"}}}]}
                (:routes (sut/find-key-routes state "@"))))))

(test/deftest accessible-keys-1
  (let [maze      (sut/initial-state-for-map (sut/read-maze sample-maze-1))
        state     (sut/find-key-routes maze "@")
        keys-left (set (keys (:key-index state)))]
    (test/is (= [["a" [2 #{{:keys-found [], :doors-blocking #{}}}]]]
                (sut/accessible-keys state keys-left "@")))))

(test/deftest solve-maze-1
  (test/is (= [8 [\a \b]] (sut/solve (sut/read-maze sample-maze-1)))))

(def sample-maze-2
  "########################
#f.D.E.e.C.b.A.@.a.B.c.#
######################.#
#d.....................#
########################
")

(test/deftest find-routes-2
  (let [state   (sut/initial-state-for-map (sut/read-maze sample-maze-2))
        state-2 (sut/find-route #{"@" "a"} state)
        state-3 (sut/find-route #{"a" "c"} state-2)]
    (test/is (= {:routes         {#{"@" "a"} [2 #{{:keys-found [], :doors-blocking #{}}}]}
                 :doors          {[3 1] "d", [5 1] "e", [9 1] "c", [13 1] "a", [19 1] "b"},
                 :steps          2,
                 :doors-blocking #{},
                 :pos            [16 1],
                 :multi-pos      [[15 1]],
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
                 :keys-found     []
                 :player         {[15 1] :player}}
                state-2))
    (test/is (= {:routes
                 {#{"a" "c"} [4 #{{:keys-found [], :doors-blocking #{"b"}}}],
                  #{"@" "a"} [2 #{{:keys-found [], :doors-blocking #{}}}]},
                 :doors          {[3 1] "d", [5 1] "e", [9 1] "c", [13 1] "a", [19 1] "b"},
                 :steps          4,
                 :doors-blocking #{"b"},
                 :pos            [20 1],
                 :multi-pos      [[15 1]],
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
                 :keys-found     []
                 :player         {[15 1] :player}}
                state-3))
    (test/is (= {#{"@" "a"} [2 #{{:keys-found [], :doors-blocking #{}}}],
                 #{"a" "c"} [4 #{{:keys-found [], :doors-blocking #{"b"}}}],
                 #{"d" "f"}
                 [44
                  #{{:keys-found     ["c" "a" "b" "e"],
                     :doors-blocking #{"d" "e" "a" "b" "c"}}}]}
                (:routes (sut/find-route #{"d" "f"} state-3))))
    (test/is (= {#{"d" "c"} [24 #{{:keys-found [], :doors-blocking #{}}}],
                 #{"f" "@"}
                 [14 #{{:keys-found ["b" "e"], :doors-blocking #{"d" "e" "a" "c"}}}],
                 #{"e" "a"} [10 #{{:keys-found ["b"], :doors-blocking #{"a" "c"}}}],
                 #{"f" "a"}
                 [16 #{{:keys-found ["e" "b"], :doors-blocking #{"d" "e" "a" "c"}}}],
                 #{"d" "@"}
                 [30 #{{:keys-found ["a" "c"], :doors-blocking #{"b"}}}],
                 #{"d" "a"} [28 #{{:keys-found ["c"], :doors-blocking #{"b"}}}],
                 #{"e" "c"}
                 [14 #{{:keys-found ["b" "a"], :doors-blocking #{"a" "b" "c"}}}],
                 #{"@" "c"} [6 #{{:keys-found ["a"], :doors-blocking #{"b"}}}],
                 #{"f" "c"}
                 [20
                  #{{:keys-found     ["e" "b" "a"],
                     :doors-blocking #{"d" "e" "a" "b" "c"}}}],
                 #{"d" "e"}
                 [38 #{{:keys-found ["c" "a" "b"], :doors-blocking #{"a" "b" "c"}}}],
                 #{"d" "b"}
                 [34 #{{:keys-found ["c" "a"], :doors-blocking #{"a" "b"}}}],
                 #{"e" "b"} [4 #{{:keys-found [], :doors-blocking #{"c"}}}],
                 #{"@" "a"} [2 #{{:keys-found [], :doors-blocking #{}}}],
                 #{"a" "b"} [6 #{{:keys-found [], :doors-blocking #{"a"}}}],
                 #{"f" "b"}
                 [10 #{{:keys-found ["e"], :doors-blocking #{"d" "e" "c"}}}],
                 #{"@" "b"} [4 #{{:keys-found [], :doors-blocking #{"a"}}}],
                 #{"e" "@"}
                 [8 #{{:keys-found ["b"], :doors-blocking #{"a" "c"}}}],
                 #{"d" "f"}
                 [44
                  #{{:keys-found     ["c" "a" "b" "e"],
                     :doors-blocking #{"d" "e" "a" "b" "c"}}}],
                 #{"b" "c"} [10 #{{:keys-found ["a"], :doors-blocking #{"a" "b"}}}],
                 #{"a" "c"} [4 #{{:keys-found [], :doors-blocking #{"b"}}}],
                 #{"f" "e"} [6 #{{:keys-found [], :doors-blocking #{"d" "e"}}}]}
                (:routes (sut/find-key-routes state "@"))))))

(test/deftest available-keys-2
  (let [maze         (sut/initial-state-for-map (sut/read-maze sample-maze-2))
        state        (sut/find-key-routes maze "@")
        keys-left    (set (keys (:key-index state)))]
    (test/is (= [["a" [2 #{{:keys-found [], :doors-blocking #{}}}]]]
                (sut/accessible-keys state keys-left "@")))
    (test/is (= [["b" [6 #{{:keys-found [], :doors-blocking #{"a"}}}]]]
                (sut/accessible-keys state (disj keys-left "a") "a")))
    (test/is (= [["d" [28 #{{:keys-found ["c"], :doors-blocking #{"b"}}}]]
                 ["c" [4 #{{:keys-found [], :doors-blocking #{"b"}}}]]]
                (sut/accessible-keys state (disj keys-left "a" "b") "a")))))

(test/deftest solve-maze-2
  (test/is (= [86 [\a \b \c \d \e \f]] (sut/solve (sut/read-maze sample-maze-2)))))

(def sample-maze-3
  "########################
#...............b.C.D.f#
#.######################
#.....@.a.B.c.d.A.e.F.g#
########################")

(test/deftest solve-maze-3
  (test/is (= [132 [\b \a \c \d \f \e \g]] (sut/solve (sut/read-maze sample-maze-3)))))

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
  (test/is (= [136 [\b \c \e \f \a \k \d \l \h \m \g \n \j \o \p \i]] (sut/solve (sut/read-maze sample-maze-4)))))

(def sample-maze-5
  "########################
#@..............ac.GI.b#
###d#e#f################
###A#B#C################
###g#h#i################
########################")

(test/deftest solve-maze-5
  (test/is (= [81 [\a \c \d \g \f \i \b \e \h]] (sut/solve (sut/read-maze sample-maze-5)))))

(test/deftest part-1
  "Test solution for part 1."
  (test/is [= [4250 [\b \c \j \e \y \f \g \s \m \t \k \a \o \w \q \h \x \z \d \v \l \i \r \u \p \n]]
            (sut/part-1)]))

;; Part 2.

(def sample-maze-2-1
  "The first sample maze for part 2."
  "###############
#d.ABC.#.....a#
######@#@######
###############
######@#@######
#b.....#.....c#
###############")

(test/deftest read-sample-maze-2-1
  "Makes sure we properly read multiple player start locations, and
  can build key paths from each."
  (let [maze (sut/initial-state-for-map (sut/read-maze sample-maze-2-1))]
    (test/is (= {[6 2] :player, [8 2] :player, [6 4] :player, [8 4] :player}
                (:player maze)))
    (test/is (= {:routes
                 {#{"d" "1"} [6 #{{:keys-found [], :doors-blocking #{"a" "b" "c"}}}],
                  #{"a" "2"} [6 #{{:keys-found [], :doors-blocking #{}}}],
                  #{"3" "b"} [6 #{{:keys-found [], :doors-blocking #{}}}],
                  #{"4" "c"} [6 #{{:keys-found [], :doors-blocking #{}}}]},
                 :multi-pos      [[6 2] [8 2] [6 4] [8 4]],
                 :doors          {[3 1] "a", [4 1] "b", [5 1] "c"},
                 :steps          6,
                 :doors-blocking #{},
                 :pos            [12 5],
                 :visited        #{[10 5] [8 4] [12 5] [8 5] [11 5] [9 5]},
                 :walls
                 #{[7 6] [7 1] [12 6] [13 3] [11 2] [4 3] [2 2] [0 0] [13 6] [1 0]
                   [2 3] [7 2] [7 4] [8 3] [0 6] [3 3] [5 4] [6 3] [0 5] [14 6] [3 4]
                   [11 0] [7 3] [8 6] [12 2] [4 2] [13 2] [3 0] [9 0] [6 6] [9 6] [5 3]
                   [9 3] [13 0] [8 0] [5 2] [4 6] [11 4] [1 4] [10 2] [12 0] [10 0]
                   [1 3] [11 6] [14 4] [11 3] [12 4] [0 3] [5 6] [14 1] [2 4] [3 6]
                   [14 5] [10 6] [9 2] [10 4] [7 0] [0 2] [2 0] [0 4] [12 3] [9 4]
                   [14 2] [1 6] [14 3] [4 4] [7 5] [2 6] [5 0] [13 4] [6 0] [1 2]
                   [10 3] [3 2] [14 0] [0 1] [4 0]},
                 :key-index      {"d" [1 1], "a" [13 1], "b" [1 5], "c" [13 5]},
                 :keys           {[1 1] "d", [13 1] "a", [1 5] "b", [13 5] "c"},
                 :player         {[6 2] :player, [8 2] :player, [6 4] :player, [8 4] :player},
                 :keys-found     []}
                (sut/find-multiplayer-key-routes maze)))))

(test/deftest accessible-keys-2-1
  (let [maze      (sut/initial-state-for-map (sut/read-maze sample-maze-2-1))
        state     (sut/find-multiplayer-key-routes maze)
        keys-left (set (keys (:key-index state)))]
    (test/is (= [["a" 1 [6 #{{:keys-found [], :doors-blocking #{}}}]]
                 ["b" 2 [6 #{{:keys-found [], :doors-blocking #{}}}]]
                 ["c" 3 [6 #{{:keys-found [], :doors-blocking #{}}}]]]
                (sut/accessible-keys-2 state keys-left ["1" "2" "3" "4"])))))

(test/deftest solve-maze-2-1
  (test/is (= [24 [\a \b \c \d]] (sut/solve-2 (sut/read-maze sample-maze-2-1)))))

(def sample-maze-2-2
  "The second sample maze for part 2."
  "#############
#DcBa.#.GhKl#
#.###@#@#I###
#e#d#####j#k#
###C#@#@###J#
#fEbA.#.FgHi#
#############")

(test/deftest solve-maze-2-2
  (test/is (= [32 [\a \b \c \d \e \f \g \h \i \j \k \l]]
              (sut/solve-2 (sut/read-maze sample-maze-2-2)))))

(def sample-maze-2-3
  "The third sample maze for part 2."
  "#############
#g#f.D#..h#l#
#F###e#E###.#
#dCba@#@BcIJ#
#############
#nK.L@#@G...#
#M###N#H###.#
#o#m..#i#jk.#
#############
")

(test/deftest solve-maze-2-2
  (test/is (= [72 [\e \a \b \h \c \d \f \g \i \k \j \l \n \m \o]]
              (sut/solve-2 (sut/read-maze sample-maze-2-3)))))

(test/deftest part-2
  "Test solution for part 2."
  (test/is [= [1640 [\r \u \j \f \h \e \g \s \b \c \m \t \y \k \a \q \w \i \o \l \x \v \p \z \d \n]]
            (sut/part-2)]))
