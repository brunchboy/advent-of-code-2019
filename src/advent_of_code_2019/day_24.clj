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

(defn bug-present
  "Given an Eris biodiversity score, returns truthy when there is a bug
  at the specified cell of the map it represents."
  [eris cell]
  (pos? (bit-and eris (bitmask-for cell))))

(defn neighbors
  "Returns all the cells adjacent to the specified one."
  [[x y]]
  (filter (fn [[x y]] (and (<= 0 x 4) (<= 0 y 4)))
          [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]]))

(defn count-occupied-neighbors
  "Returns the numbers of cells adjacent to the specified cell that
  currently have bugs in them for a given Eris biodiversity score."
  [eris cell]
  (count (filter (partial bug-present eris) (neighbors cell))))

(defn fate
  "Given a current Eris state and a cell coordinate, returns zero if
  that coordinate will be empty in the next generation, or its
  contribution to the biodiversity score if there will be a bug
  there."
  [eris cell]
  (let [crowd (count-occupied-neighbors eris cell)]
    (if (bug-present eris cell)
      (if (= 1 crowd) (bitmask-for cell) 0)  ; Bugs survive with exactly one neighbor.
      (if (<= 1 crowd 2) (bitmask-for cell) 0))))  ; Empty cells get infested with one or two neighbors.

(defn generation
  "Given a current state, returns the state that follows it, given the
  life and death rules in the problem statement."
  [eris]
  (reduce + (for [x (range 5)
                  y (range 5)]
              (fate eris [x y]))))
