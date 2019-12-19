(ns advent-of-code-2019.day-18
  "Solutions to the Day 18 problems."
  (:require [clojure.repl :refer :all]))

(defn describe-maze-cell
  "Translates a character found in the maze to the top-level key under
  which it should be stored, and any additional value needed to
  describe it."
  [c]
  (cond
    (= c \@)
    [:player :player]

    (= c \#)
    [:walls :wall]

    (Character/isLowerCase c)
    [:keys (str c)]

    (Character/isUpperCase c)
    [:doors (clojure.string/lower-case (str c))]

    (= c \.)
    nil

    :else
    (throw (ex-info (str "Unrecognized maze character, " c) {:char c}))))

(defn read-maze-line
  "Reads a single line of the maze, returning tuples describing the
  non-empty cells found in it. The first part of the tuple is the list
  of keys under which the value should be stored in the map, and the
  second part is the value to store."
  [y line]
  (filter identity (map-indexed (fn [x c]
                                  (when-let [[k v] (describe-maze-cell c)]
                                    [[k [x y]] v]))
                                line)))

(defn read-maze
  "Reads the string that describes a maze, and creates the map that
  describes it."
  [maze-str]
  (reduce (fn [m [ks v]] (assoc-in m ks v))
          {} (apply concat (map-indexed read-maze-line (clojure.string/split-lines maze-str)))))

(defn position-after-move
  "Updates coordinates based on the direction moved."
  [x y direction]
  (case direction
    :north [x (dec y)]
    :south [x (inc y)]
    :west [(dec x) y]
    :east [(inc x) y]))

(declare visit)

(def best-steps-for-key-orderings
  (atom {}))

(def best-solutions
  (atom [Long/MAX_VALUE #{}]))

(defn try-direction
  "Checks what lies in a particular direction from the current
  coordinates, and proceeds accordingly."
  [x y steps state direction]
  #_(println "trying" x y steps direction)
  (let [[x y]     (position-after-move x y direction)
        key-found ((:keys state) [x y])]
    (cond
      (or ((:walls state) [x y]) ((:visited state) [x y]) ((:doors state) [x y]))
      Long/MAX_VALUE ; We can't move this direction, at least not yet.

      (> steps (first @best-solutions))
      (do
        #_(println "Abandoning branch because slower than existing best solutions.")
        Long/MAX_VALUE)

      key-found
      (let [keys-found  (conj (:keys-found state) key-found)
            best-so-far (@best-steps-for-key-orderings keys-found Long/MAX_VALUE)]
        #_(println "Best so far:" best-so-far)
        (if (<= best-so-far steps)
          (do
            #_(println "Abandoning branch because already have faster way to find keys" keys-found)
            Long/MAX_VALUE)  ; We already found a better or equally good way to use this key, so give up this branch.
          (let [state (-> state  ; We found a better way to use this key.
                          (update :keys dissoc [x y])  ; Note that it has been picked up.
                          (update :keys-found conj key-found)  ; Track all the keys we have found so far.
                          (update :doors dissoc ((:door-index state) key-found))  ; Open (remove) corresponding door.
                          (assoc :visited #{}))]  ; Reset our visited markers because the maze has changed.
            (swap! best-steps-for-key-orderings assoc keys-found steps)  ; Record steps needed to get these keys.
            #_(println "Found keys" (:keys-found state) "at steps" steps)
            #_(println state)
            (if (empty? (:keys state))
              (do
                (println "Found best-so-far solution! Steps:" steps "keys:" keys-found)
                (swap! best-solutions (fn [[solution-steps solution-keys-found]]
                                        (if (< steps solution-steps)
                                          [steps #{keys-found}]
                                          [solution-steps (conj solution-keys-found keys-found)])))
                steps)  ; We have solved the maze by finding the last key!
              (visit x y steps state))))) ; And try solving the new maze from here.

      :else
      (visit x y steps state))))  ; An ordinary move, continue solving from here.

(defn visit
  "Recursive shortest path maze solver. Steps are how many steps have
  been taken so far, and state is the current map state (which
  includes keys found and remaining, as well as visited squares). When
  we find a key, we update the state to remove all our visited marks
  as well as the corresponding door, because we basically need to
  re-solve the maze from that point. Returns the shortest number of
  steps required to get to a state where all keys have been found, or,
  `Long/MAX_VALUE` if it cannot be solved from this state."
  [x y steps state]
  #_(println x y steps visited (visited [x y]))
  (apply min (map (partial try-direction x y (inc steps) (update state :visited conj [x y]))
                  [:north :south :east :west])))

(defn initial-state-for-map
  "Converts a maze map into the initial state needed by the solver.
  Removes the player entry, sets up the visited set, turns the walls
  entry into a simple set of coordinates, and builds an index from
  door name to its coordinate so when the key is found the door can
  easily be removed."
  [maze]
  (-> maze
      (dissoc :player)  ; This is just used to seed the solver, below.
      (assoc :visited #{})  ; Keep track of places we have been.
      (assoc :door-index (clojure.set/map-invert (:doors maze)))  ; Make it easy to open them.
      (assoc :keys-found [])  ; Keep track of the keys we have found along this path, in order.
      (update :walls (fn [m] (set (keys m))))))  ; All we need is the set of coordinates.

(defn solve
  "Sets up the state given the map that was read, and runs the solver."
  [maze]
  (reset! best-solutions [Long/MAX_VALUE #{}])
  (reset! best-steps-for-key-orderings {})
  (let [state (initial-state-for-map maze)
        [x y] (first (keys (:player maze)))
        steps (visit x y 0 state)]
    (if (= steps Long/MAX_VALUE)
      :failed
      steps)))

(def part-1-maze
  "The maze for part 1 of the problem."
  "#################################################################################
#...#.......#....a..#...........#..e....#.....#...#...#...........#.............#
#.#.#.#####.#.#####.#.#######.###.###.#.#.###.#.#.###.#.#########.#.###.#######.#
#.#.#.#.#...#.#.K...#...#...#.....#.#.#.#.#.....#.#...#t......#...#.#...#.......#
#.###B#.#.#.#.#.#######.###.#######.#.###.#######.#.#########.#.#####.###.#######
#.#q..#.#.#.#.#...#.....#...#.......#...#...#.#...#.........#.#.......#.#.#.....#
#.#.###.#.#.#.###.#.#####.#.#.#####.###.###.#.#.#####.#######.#########.#.###.#.#
#...#...#.#.#...#.#.......#...#.....#...#...#.........#.....#.......#.#...#m..#.#
#.#####.#.#####.#.#######.#########.#.###F#############.###.###.###.#.#.###.#####
#...#...#.......#.......#.#......h#.#...#.#.....#.......#.....#.#...#.#.#.......#
###.#.###########.#####.#.#.#####.#####.#.#.###.#.#####.#####.#.#.###G#.###.###.#
#.#.#.......#...#...#...#.#...J.#.#.....#.#...#.#.....#.#...#.#.#.#...#...#...#.#
#.#.#####.###.#.###.#####.#####.#.#.###.#.###.#.#####.#.#.#.#.###.#.#.###.#####S#
#w#...#...#...#...#...#...#...#.#...#...#.#...#.....#.#.#.#.#.....#.#...#.......#
#.###.#.###.#####.###.#.#####.#.#####.###.#.###.###.#.###.#.#######.###.#######.#
#...#...#...#.....#...#.#...#.#...#.#.#.#.#.#.#.#...#.....#...#...#.#.#.........#
#.#####.#.###.#####.###.#.#.#.###.#.#.#.#.#.#.#.###.#########.#.#.#.#.###########
#.......#...#.....#.#.....#...#.#.#...#.#...#.#...#.....#...#.#.#.#.....#.......#
#C#########.#####.#.#.#######.#.#.###.#.#.###.###.#####.#.###.#.#######.###.###.#
#.#.......#.#.#...#...#...#.....#...#...#.......#.#.....#.#...#.....#.#...#...#.#
#.###.#.#.#.#.#.###.###.#.#########.###.#####.###.#.#####.#.#####.#.#.###.###.###
#...#.#.#.#.#.#.#y..#...#...#.....#.#...#.....#...#.#.....#...#...#.....#...#...#
###.###.#I#.#.#.###########.#.###.#.#####.#####.###.#.###.#####.###########.#.#.#
#...#...#.#.#.#...........#...#...#.#...#...#...#.#...#...#.....#.........#.#.#.#
#.###.###.#.#.#####.###########.###.#.#.#####.###.#.#####.###.###.#######.#.###.#
#.....#...#...#...#.............#.....#.#.....#...#.#...#.....#...#.....#...#...#
#####.#######.#.#.###############.#####.#.#######.#.#.#.#####.#.###.###.#####.###
#.....#.....#...#.#...#.............#...#.......#...#.#.#.....#.#.#.#.#.........#
#.#####.###.#####.#.###.###########.#.###.#####.#####.#.#######.#.#.#.#########.#
#...#.#.#.#.......#...........#...#.#...#.....#.......#...#.....#...#.....#...#.#
###.#.#.#.###############.#####.#.#.###.#####.###########.#.#######.###.###.#.#.#
#...#.#...#.......#.....#.#.....#.#...#.#.....#...#.....#...#.....#.#...#...#...#
#.###.###.#.#####.#.###.###.#####.#####.#.#####.###.###.#####.###.#.#.###.#####.#
#.#.....#.#...#.....#...#...#.....#.....#...#.......#...#.....#.#.#...#...#.....#
#.###L#.#.#.#.#######.###.###.#####.###.###.#.#######.#.###.###.#.###.#.###.#####
#o..#.#.#.#.#.....#.#.#...#.#...#...#.#.#.#.#.....#...#.....#.#...#...#.#.......#
###.###.#.#######.#.#.#.#.#.###.#.###.#.#.#.#######.#########.#.#######.#######.#
#.#...#.#.......#.#.#.#.#.#.#...#.#...#.#.#.#...#...#.........#.#.....#.......#.#
#.###.#.#######.#.#.#.###.#.#.###.#.###.#.#.#.#.#.###.#######.#.#.###.#######.#.#
#.............#...#.......#.......#...........#...#.........#.....#...........#.#
#######################################.@.#######################################
#.....#.....#.#.........#.#...#.....#...........#..u........#...#.......#.....Q.#
#.#.###.#.#.#.#.#######.#.#.#.#.#.###.#.#.#.###.#.#########.###.#.#.###.#######.#
#.#.#...#.#.#.#.....#...#.R.#.#.#.....#.#.#...#...#.......#r....#.#...#.........#
#.#.#.###.#.#.#####.###.#####.#.#######.#.###.#######.###.#####.#.###.#####.#####
#.#...#...#.......#...#.....#.#.#.#...#.#.#.#...#...#.#...#.....#b#.#...#...#..c#
#.#####.#############.#####.#.#.#.#.#.#.#.#.###.#.#.#.#.###.#####.#.###.#####.#.#
#p#...V.#..x#.......#...#...#.....#.#...#.#...#.#.#...#...#.#.#...#...#.....#.#.#
###.#####.#.#.#####.###.#.###U#####.###.#.#.#.#.#.#######.#.#.#.###.#.#####.#.#.#
#...#.....#.....#...#...#.#.#.#.....#.#.#...#.#.#.#...#.#.#...#...#.#.....#...#.#
#.###.###########.###.###.#.#.#.#####.#.#.#####.#.#.#.#.#.###.###.#.#####.#####.#
#.......#.......#...#.#.#.#...#...#.....#.#...#.#.#.#.#...#...#.#.#.....#...#.#.#
#.#####.#.#####.###.#.#.#.#.#####.#######.#.#.#.#.#H#.###.#.###.#.#.###.###.#.#.#
#.#...#.#.#...#.....#.#...#.....#...#...#.#.#...#.#.#...#.#.....#.#...#.#.#...#.#
#.#.#.###.#.#######.#.###.#########.#.#.###.#####.#.###.###.#####.#####.#.###.#.#
#.#.#.....#...#.....#...#.#.......#...#.#...#.....#.#.#.....#...#.......#.....#.#
#.#.#########.#.#######.#.#.#####.#####.#.###.#.###.#.#######.#.#########.#####.#
#.#.........#.#.#.#.....#...#...#.....#.#...#.#.#.........#...#...#.....#...#...#
#.#########.#.#.#.#.#########.#.#####.#.#.#.#.###.#########.###.###.#.#####.#.#.#
#.#.......#.#.#...#.#.......#.#.....#.#.#.#.#.....#.#.........#.#...#.#.....#.#.#
#.#######.#.#.###.#.###.#####.#####.#.#.#.#.###.###.#.#########.#.###.#.#####.#.#
#......z..#.#.#...#...#.....#.#.....#.#.#.#...#.....#.......#.#.#.#.#...#...#.#.#
#########.#.#.#.#######.###.#.#.###.#.#.#.###.###########.###.#.#.#.#######.#.#.#
#.#.......#.#...#.....#.#.#.#.#.#...#.#.#.#.#.............#...#.#.......#...#.#.#
#.#.#######.#.###.#.#.#.#.#.#.#.#.###.#.#.#.###############.###.#######.#.###.#.#
#...#.......#...#.#.#.#...#...#.#...#.#.#....j#...#...#....d#...#...#...#...#.#.#
#.###.#########.###.#.###.#####.#####.#.#####.#.###.#.#.#######.#.#.#.#####.#.#.#
#...#.#.......#.#...#...#...#.#.....#...#...#.#.....#.#.Z.#.....#.#...#.....#.#.#
#D###.#####.###.#.#####.###.#.#####.###.###.#.#######.###.###.###.#######.###.#.#
#.#...#...#..n#...#.N.#.#...#.....#.....#...#.....#...#.#...#.....#.....#.#...#.#
###.###.#.###.#######.#.#.#####.#.#######.#######.#.#.#.###.#.#####.###.#.#.###W#
#...#...#.............#.#.....#.#.....M.#.#.......#.#...#...#.......#.....#...#.#
#.###.#.###############.#####.###.#####.#.#.#######.###.#.#######.#####.#####.#.#
#.#.P.#...#...#.......#.#...#.....#...#.#.#...#.....#...#...#.#...#...#.#.....#i#
#.#######.#.#.#.#####.#.#.#.#######.#.#.#.###.###.###.#####.#.#.###.#.###.#####.#
#.......#...#.......#...#.#.......#.#...#...#....f#.E.#...#.#.....#.#.#...#.O.#.#
#.#####.#################.###.#####.#####.#########.#####.#.#######X#.#.###.#.###
#.#.#...#...#...T...#.A.#...#..k..#.#...#.#...#...#.#...#.#.....#...#.#.....#...#
#.#.#.###.#.#.#####.#.#.###.#####.#.#.#.#.#.#.#.#.#.#.#.#.#####.#.###.#########.#
#...#.....#.......#...#.........#.Y...#.#..s#...#..g..#.......#v..#............l#
#################################################################################")

(defn part-1
  "Solve part 1."
  []
  (solve (read-maze part-1-maze)))
