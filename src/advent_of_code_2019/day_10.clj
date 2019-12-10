(ns advent-of-code-2019.day-10
  "Solutions to the Day 10 problems."
  (:require [clojure.repl :refer :all]))

(defn read-map-line
  "Converts a single line of the ASCII art asteroid map to a list of the
  coordinates where asteroids are present."
  [y line]
  (filter identity (map-indexed (fn [x c] (when (= \# c) [x y])) line)))

(defn read-map
  "Converts an ASCII art asteroid map to a corresponding list of
  coordinates."
  [art]
  (apply concat (map-indexed read-map-line (clojure.string/split-lines art))))

(def two-pi
  "Holds two times pi for computations."
  (* Math/PI 2.0))

(def half-pi
  "Holds 1/2 pi for computations."
  (/ Math/PI 2.0))


(defn angles-from
  "Given a list of asteroid coordinates and an origin asteroid, returns
  a list of the angles at which the other asteroids appear from the
  origin. If the origin is present in the list, it is skipped. We also
  calculate the distance to the asteroid in case part 2 might want it."
  [origin asteroids]
  (filter identity
          (for [target asteroids]
            (when (not= target origin)
              (let [[origin-x origin-y] origin
                    [target-x target-y] target
                    x                   (double (- origin-x target-x))
                    y                   (double (- origin-y target-y))
                    r                   (Math/sqrt (+ (* x x) (* y y)))
                    θ                   (Math/atan2 y x)]
                [target-x target-y θ r])))))

(defn num-visible-from
  "Given a list of asteroid coordinates and an origin asteroid, returns
  the number that can be seen unobstructed from there."
  [origin asteroids]
  (let [angled (angles-from origin asteroids)]
    (count (set (map #(nth % 2) angled)))))

(defn find-best
  "Examines all the asteroids in the map, determining the number of
  other asteroids that can be seen from them, then finds the one from
  which the most can be seen."
  [asteroids]
  (let [counted (map (fn [asteroid] [(num-visible-from asteroid asteroids) asteroid]) asteroids)]
    (reduce (fn [a b]
              (if (> (first a) (first b)) a b))
            counted)))

(def part-1-map
  "The asteroid map for part 1."
  "#..#.#.###.#...##.##....
.#.#####.#.#.##.....##.#
##..#.###..###..#####..#
####.#.#..#....#..##.##.
.#######.#####...#.###..
.##...#.#.###..###.#.#.#
.######.....#.###..#....
.##..##.#..#####...###.#
#######.#..#####..#.#.#.
.###.###...##.##....##.#
##.###.##.#.#..####.....
#.#..##..#..#.#..#####.#
#####.##.#.#.#.#.#.#..##
#...##.##.###.##.#.###..
####.##.#.#.####.#####.#
.#..##...##..##..#.#.##.
###...####.###.#.###.#.#
..####.#####..#####.#.##
..###..###..#..##...#.#.
##.####...##....####.##.
####..#..##.#.#....#..#.
.#..........#..#.#.####.
###..###.###.#.#.#....##
########.#######.#.##.##")

;; Part 2

(defn normalized-angles-from
  "Rotates the angles of the asteroids so that straight up is zero and
  they are all positive, so they can be sorted in the vaporization
  order specified by the problem statement."
  [origin asteroids]
  (map (fn [[x y θ r]] [(mod (- θ half-pi) two-pi) r x y])
       (angles-from origin asteroids)))

(defn assign-passes
  "Adds a pass number at the front of the angled asteroid list,
  recording on which pass the laser will be able to vaporize an
  asteroid if it is at the same angle as another (closer ones get
  vaporized on earlier passes). This allows the entire list to be
  sorted in proper vaporization order."
  [angled]
  (loop [[θ r x y] (first angled)
         pass      1
         left      (rest angled)
         result    []]
    (if (nil? θ)
      result
      (recur (first left)
             (if (not= θ (first (first left))) 1 (inc pass))
             (rest left)
             (conj result [pass θ r x y])))))

(defn vaporization-order
  "Sorts the asteroids into a table in the order in which they will be
  vaporized by the terms of the problem rules. The table rows are
  tuples of the pass number in which the laser its them, the angle at
  which it is pointing (clockwise from noon), the distance to the
  asteroid, and the x and y coordinates."
  [origin asteroids]
  (sort (assign-passes (sort (normalized-angles-from origin asteroids)))))
