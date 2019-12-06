(ns advent-of-code-2019.day-3
  "Solutions to the Day 3 problems"
  (:require [clojure.repl :refer :all]))

(defn extract-segment
  "Pulls off and decomposes the first wire routing."
  [specs]
  (let [current   (first specs)
        direction (subs current 0 1)
        distance  (Long/valueOf (subs current 1))]
    [direction distance (rest specs)]))

(defn gather-segments
  "Collects horizontal and vertical line segments given wire routing.
  Returns a vector whose first element is the vector of horizontal
  line segments, and whose second is the vector of vertical line
  segments.

  Horizontal line segments are represented as `[y [x-min x-max]]` and
  vertical segments as `[x [y-min y-max]]` for ease of detecting
  intersections."
  [routing]
  (loop [x          0
         y          0
         horizontal []
         vertical   []
         specs      (clojure.string/split routing #",")]
    (if (seq specs)
      (let [[direction distance remaining] (extract-segment specs)]
        (case direction
          "R" (recur (+ x distance) y (conj horizontal [y [x (+ x distance)]]) vertical remaining)
          "L" (recur (- x distance) y (conj horizontal [y [(- x distance) x]]) vertical remaining)
          "U" (recur x (+ y distance) horizontal (conj vertical [x [y (+ y distance)]]) remaining)
          "D" (recur x (- y distance) horizontal (conj vertical [x [(- y distance) y]]) remaining)))
      [horizontal vertical])))

(defn find-segment-intersections
  "Finds points where wire segments intersect, given horizontal segments
  of one and vertical segments of the other."
  [h-segments v-segments]
  (filter identity
          (for [[h-y [h-x1 h-x2]] h-segments
                [v-x [v-y1 v-y2]] v-segments]
            (when (and (<= v-y1 h-y v-y2)
                       (<= h-x1 v-x h-x2))
              [v-x h-y]))))

(defn find-intersections
  "Given a pair of wire specifications, return a list of points where
  their segments intersect each other."
  [wire-1 wire-2]
  (let [segments-1 (gather-segments wire-1)
        segments-2 (gather-segments wire-2)]
    (concat (find-segment-intersections (first segments-1) (second segments-2))
            (find-segment-intersections (first segments-2) (second segments-1)))))

(defn manhattan-distance
  "Calculates the manhattan distance of a point from the origin."
  [[x y]]
  (+ (Math/abs x) (Math/abs y)))

(defn closest-intersection
  "Solves the problem by finding the intersection between two wire
  specifications that is closest to the origin without being the
  origin."
  [wire-1 wire-2]
  (apply min (filter pos? (map manhattan-distance (find-intersections wire-1 wire-2)))))

;; Part 2

(defn gather-segments-with-steps
  "Collects horizontal and vertical line segments given wire routing.
  Returns a pair of vectors of segment vectors, much like
  `gather-segments`, but each segment has two additional elements: the
  total step count taken to reach the start of the segment, and a
  function which takes a position along the segment (an x value for
  horizontal segments, and a y value for vertical segments) and
  returns how many additional steps are required to reach that
  location."
  [routing]
  (loop [x          0
         y          0
         steps      0
         horizontal []
         vertical   []
         specs      (clojure.string/split routing #",")]
    (if (seq specs)
      (let [[direction distance remaining] (extract-segment specs)]
        (case direction
          "R" (recur (+ x distance) y (+ steps distance)
                     (conj horizontal [y [x (+ x distance)]
                                       (fn [x2] (+ steps (- x2 x)))])
                     vertical remaining)
          "L" (recur (- x distance) y (+ steps distance)
                     (conj horizontal [y [(- x distance) x]
                                       (fn [x2] (+ steps (- x x2)))])
                     vertical remaining)
          "U" (recur x (+ y distance) (+ steps distance) horizontal
                     (conj vertical [x [y (+ y distance)]
                                     (fn [y2] (+ steps (- y2 y)))])
                     remaining)
          "D" (recur x (- y distance) (+ steps distance) horizontal
                     (conj vertical [x [(- y distance) y]
                                     (fn [y2] (+ steps (- y y2)))])
                     remaining)))
      [horizontal vertical])))

(defn find-segment-intersections-with-steps
  "Finds points where wire segments intersect, given horizontal segments
  of one and vertical segments of the other. The value returned is the
  x and y coordinates of the intersections, and the number of steps it
  took to get there along each segment's wire."
  [h-segments v-segments]
  (filter identity
          (for [[h-y [h-x1 h-x2] h-step-fn] h-segments
                [v-x [v-y1 v-y2] v-step-fn] v-segments]
            (when (and (<= v-y1 h-y v-y2)
                       (<= h-x1 v-x h-x2))
              [v-x h-y (h-step-fn v-x) (v-step-fn h-y)]))))

(defn find-intersections-with-steps
  "Given a pair of wire specifications, return a list of points where
  their segments intersect each other, along with the number of steps
  along each wire were required to reach that point."
  [wire-1 wire-2]
  (let [segments-1 (gather-segments-with-steps wire-1)
        segments-2 (gather-segments-with-steps wire-2)]
    (concat (find-segment-intersections-with-steps (first segments-1) (second segments-2))
            (find-segment-intersections-with-steps (first segments-2) (second segments-1)))))

(defn closest-intersection-with-steps
  "Solves the second version of the problem by finding the intersection
  between two wire specifications that requires the fewest total steps
  along the wires to reach."
  [wire-1 wire-2]
  (apply min (filter pos? (map (fn [[x y steps-1 steps-2]] (+ steps-1 steps-2))
                               (find-intersections-with-steps wire-1 wire-2)))))
