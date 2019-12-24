(ns advent-of-code-2019.day-24
  "Solutions to the Day 24 problems."
  (:require [clojure.repl :refer :all]))

(defn bitmask-for
  "Returns the bit mask corresponding to a given coordinate in an Eris
  biodiversity score."
  [[x y]]
  (bit-shift-left 1 (+ x (* y 5))))

(defn read-map-line
  "Reads a single line of an Eris map, returning its biodiversity
  score."
  [y line]
  (reduce + (map-indexed (fn [x c]
                           (if (= \# c)
                             (bitmask-for [x y])
                             0))
                         line)))

(defn read-map
  "Reads an Eris map, returning its biodiversity score."
  [eris]
  (reduce + (map-indexed read-map-line (clojure.string/split-lines eris))))

(defn bug-present?
  "Given an Eris biodiversity score, returns truthy when there is a bug
  at the specified cell of the map it represents."
  [eris cell]
  (pos? (bit-and eris (bitmask-for cell))))

(defn neighbors
  "Returns all the cells adjacent to the specified one."
  [[x y]]
  (filter (fn [[x y]] (and (<= 0 x 4) (<= 0 y 4)))
          [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]))

(def neighbors-memo
  (memoize neighbors))

(defn count-occupied-neighbors
  "Returns the numbers of cells adjacent to the specified cell that
  currently have bugs in them for a given Eris biodiversity score."
  [eris cell]
  (count (filter (partial bug-present? eris) (neighbors-memo cell))))

(defn fate
  "Given a current Eris state and a cell coordinate, returns zero if
  that coordinate will be empty in the next generation, or its
  contribution to the biodiversity score if there will be a bug
  there."
  [eris cell]
  (let [crowd (count-occupied-neighbors eris cell)]
    (if (bug-present? eris cell)
      (if (= 1 crowd) (bitmask-for cell) 0)  ; Bugs survive with exactly one neighbor.
      (if (<= 1 crowd 2) (bitmask-for cell) 0))))  ; Empty cells get infested with one or two neighbors.

(defn generation
  "Given a current state, returns the state that follows it, given the
  life and death rules in the problem statement."
  [eris]
  (reduce + (for [x (range 5)
                  y (range 5)]
              (fate eris [x y]))))

(def part-1-map
  "The state of Eris in the setup of Part 1."
  "..#.#
#.##.
.#..#
#....
....#")

(defn part-1
  "Solve part 1."
  []
  (loop [seen  #{}
         eris (read-map part-1-map)]
    (if (seen eris)
      eris
      (recur (conj seen eris)
             (generation eris)))))

(defn augment-map
  "Adds structures to the part map to support the efficient calculation
  of the recursive rules: `:levels` holds a map keyed by level number
  whose value is the biodiversity score for that level (the middle
  square always has value 0), and `:min-level` and `:max-level` track
  the lowest and highest levels that bugs have reached, to avoid
  having to scan the map to determine those values."
  [eris]
  {:levels {0 eris}
   :min-level 0
   :max-level 0})

(defn add-level
  "Grows a new level in the recursive map. `where` is either
  `:min-level` to grow downwards, or `:max-level` to grow upwards.
  Updates the min/max value appropriately, and stores the new bug grid
  at that index."
  [eris where grid]
  (let [new-index (case where
                    :min-level (dec (:min-level eris))
                    :max-level (inc (:max-level eris)))]
    (-> eris
        (assoc where new-index)
        (assoc-in [:levels new-index] grid))))

(defn neighbors-below
  "Returns all the cells adjacent, in the next level down, to the
  specified cell."
  [[x y]]
  (filter identity
          (concat [(when (zero? x) [1 2])
                   (when (zero? y) [2 1])
                   (when (= 4 x) [3 2])
                   (when (= 4 y) [2 3])])))

(def neighbors-below-memo
  (memoize neighbors-below))

(defn neighbors-above
  "Returns all the cells adjacent, in the next level up, to the
  specified cell."
  [[x y]]
  (case [x y]
    [1 2] (for [y (range 5)] [0 y])
    [2 1] (for [x (range 5)] [x 0])
    [3 2] (for [y (range 5)] [4 y])
    [2 3] (for [x (range 5)] [x 4])
    []))

(def neighbors-above-memo
  (memoize neighbors-above))

(defn level-count-occupied-neighbors
  "Returns the numbers of cells adjacent to the specified cell that
  currently have bugs in them for a given grid and the levels above
  and below."
  [grid cell above below]
  (+ (count (filter (partial bug-present? grid) (neighbors-memo cell)))
     (count (filter (partial bug-present? above) (neighbors-above-memo cell)))
     (count (filter (partial bug-present? below) (neighbors-below-memo cell)))))

(defn level-fate
  "Given a current grid state, cell coordinate, and grids above and
  below it, returns zero if that coordinate will be empty in the next
  generation, or its contribution to the biodiversity score if there
  will be a bug there."
  [grid cell above below]
  (if (= cell [2 2])
    0 ; The center cell is always empty at this level because it holds the next level down.
    (let [crowd (level-count-occupied-neighbors grid cell above below)]
      (if (bug-present? grid cell)
        (if (= 1 crowd) (bitmask-for cell) 0)  ; Bugs survive with exactly one neighbor.
        (if (<= 1 crowd 2) (bitmask-for cell) 0)))))  ; Empty cells get infested with one or two neighbors.

(defn level-generation
  "Calculates the next-generation rules for a single level in the
  recursive case, considering the levels immediately above and below
  the one being updated. `level` is the one being worked on, and
  `grid` holds its biodiversity score at the start of the round."
  [eris level grid]
  (let [below (get-in eris [:levels (dec level)] 0)
        above (get-in eris [:levels (inc level)] 0)]
    (reduce + (for [x (range 5)
                    y (range 5)]
                (level-fate grid [x y] above below)))))


(defn generation-2
  "Given a current state of the recursive grids described in part 2,
  compute the next state, which may add bugs to levels that were
  previously unused but adjacent to occupied ones."
  [eris]
  ;; First, apply the survival/birth rules on each level that already
  ;; had bugs, and keep track of the lowest and highest levels
  (let [existing (reduce (fn [result [level grid]]
                           (assoc result level (level-generation eris level grid)))
                         {}
                         (:levels eris))]
    ;; Then, see if we have any bugs at the top or bottom boundary
    ;; layers that were previously empty. If so, expand the map to
    ;; hold them.
    (let [grew-down (level-generation eris (dec (:min-level eris)) 0)
          grew-up (level-generation eris (inc (:max-level eris)) 0)]
      (cond-> (assoc eris :levels existing)
        (pos? grew-down)
        (add-level :min-level grew-down)

        (pos? grew-up)
        (add-level :max-level grew-up)))))

(defn count-bugs-in-grid
  "Calculates the number of bugs present on a single recursive layer."
  [grid]
  (count (filter (partial bug-present? grid) (for [x (range 5)
                                                  y (range 5)]
                                              [x y]))))

(defn count-bugs
  "Counts the bugs on all the levels."
  [eris]
  (apply + (map count-bugs-in-grid (vals (:levels eris)))))

(defn part-2
  "Solve part 2."
  []
  (let [eris (augment-map (read-map part-1-map))]
    (count-bugs (nth (iterate generation-2 eris) 200))))

(defn print-grid
  "Prints a grid of cells in the format of the example for part 2, but
  with an X in the center if I have erroneously placed a bug there."
  [grid]
  (doseq [y (range 5)]
    (doseq [x (range 5)]
      (print (if (= [x y] [2 2])
               (if (bug-present? grid [x y]) \X \?)
               (if (bug-present? grid [x y]) \# \.))))
    (println)))

(defn print-levels
  "Shows the levels present in the format of the examples."
  [eris]
  (doseq [level (range (:min-level eris) (inc (:max-level eris)))]
    (println "Depth" level)
    (print-grid (get-in eris [:levels level]))
    (println)))
