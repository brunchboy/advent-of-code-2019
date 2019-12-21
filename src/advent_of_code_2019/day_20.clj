(ns advent-of-code-2019.day-20
  "Solutions to the Day 20 problems."
  (:require [clojure.repl :refer :all]))

(defn position-after-move
  "Updates coordinates based on the direction moved."
  [x y direction]
  (case direction
    :north [x (dec y)]
    :south [x (inc y)]
    :west [(dec x) y]
    :east [(inc x) y]))

(defn find-label
  [lines x y])

(defn process-label
  "Checks if the specified maze cell, which is already known to be a
  letter, is the start of a label. It must be adjacent to another
  letter in one of the cardinal compass directions, and the next cell
  after that in the same direction must be a path cell. If that is all
  true, adds the position of the second letter to the maze as either a
  portal with the specified label, or as the start cell (if the label
  was AA), or the goal cell (if the label was ZZ). Otherwise leaves
  the maze unchanged."
  ([lines x y maze]  ; Try all four directions
   (reduce (partial process-label lines x y) maze [:north :south :east :west]))
  ([lines x y maze direction]  ; See this is the start of a label that reaches the path in the specified direction.
   (let [letter-1        (get-in lines [y x])
         [x y]           (position-after-move x y direction)
         letter-2        (get-in lines [y x] \ )
         [path-x path-y] (position-after-move x y direction)
         path            (get-in lines [path-y path-x] \ )]
     (if (and (= \. path)
                (Character/isLetter letter-2))
       (let [label (if (#{:south :east} direction)
                     (str letter-1 letter-2)
                     (str letter-2 letter-1))]
         (case label
           "AA" (assoc maze :start [path-x path-y])
           "ZZ" (assoc maze :goal [path-x path-y])
           (update-in maze [:portals label] (fnil conj []) [[x y] [path-x path-y]])))
       maze))))

(defn process-maze-cell
  "Given the lines of the textual maze map, the current state of the
  maze, and a coordinate pair, updates the maze state to reflect what
  is found at those coordinates."
  [lines maze [x y]]
  (let [c (get-in lines [y x] \ )]
    (cond
      (= c \.) ; We found a path cell.
      (update maze :path conj [x y])

      (Character/isLetter c)
      (process-label lines x y maze) ; Find the adjacent path and the full name and build a portal or start/end.

      :else
      maze))) ; Something we don't care about

(defn add-portal-links
  "Adds an index from each cell that is a portal entrance to the portal
  exit to make it easy to traverse"
  [maze]
  (reduce (fn [result [_ [side-a side-b]]]
            (-> result
                (assoc-in [:portals (first side-a)] (second side-b))
                (assoc-in [:portals (first side-b)] (second side-a))))
          maze
          (:portals maze)))

(defn read-maze
  "Reads the string that describes a maze, and creates the map that
  describes it."
  [maze-str]
  (let [lines (clojure.string/split-lines maze-str)
        maze  (reduce (partial process-maze-cell lines)
                      {:path    #{}
                       :portals {}
                       :visited #{}
                       :steps   0}
                      (for [y (range (count lines))
                            x (range (apply max (map count lines)))]
                        [x y]))]
    (add-portal-links maze)))

(declare visit)

(defn traverse-portals
  "If the specified position is the entrance of a portal, returns the
  exit end of the portal. Otherwise returns the original position."
  [{:keys [portals]} position]
  (if-let [other-end (portals position)]
    other-end
    position))

(defn try-direction
  "Checks what lies in a particular direction from the current
  coordinates (including passage through a portal) and proceeds
  accordingly. Returns with an updated state reflecting the best
  outcome that can be obtained in that direction."
  [{:keys [steps path visited goal portals]
    [x y] :pos
    :as   state}
   direction]
  (let [[x y] (traverse-portals state (position-after-move x y direction))]
    (cond
      (= [x y] goal)
      state  ; We found a solution!

      (and (path [x y]) (not (visited [x y])))
      (visit (assoc state :pos [x y])) ; A move to a new square, continue solving from here.

      :else ; We can't move in that direction
      (assoc state :steps Long/MAX_VALUE))))

(defn best-result
  "Given two solution states, returns the one with the smallest step count."
  [state-1 state-2]
  (if (< (:steps state-1) (:steps state-2))
    state-1
    state-2))

(defn visit
  "Recursive shortest path maze solver, including arbitrary use of
  portals, as specified by part 1."
  [{[x y] :pos
    :as   state}]
  (reduce best-result
          (map (partial try-direction
                        (-> state
                            (update :steps inc)
                            (update :visited conj [x y])))
               [:north :south :east :west])))

(defn solve
  "Sets up the state given the map that was read, and runs the solvers."
  [maze]
  (let [state (visit (assoc maze :pos (:start maze)))
        steps (:steps state)]
    (if (= steps Long/MAX_VALUE)
      :failed
      steps)))

;; Part 2

(defn read-maze-2
  "Reads the lines of the maze as before, then adds more information to
  the state to support the additional rules stipulated in part 2.
  Separates the portals into descending and ascending, adds a
  current-level value, and a set of descending portals that have been
  visited."
  [maze-text]
  (let [maze             (read-maze maze-text)
        maze-lines       (clojure.string/split-lines maze-text)
        max-x            (dec (apply max (map count maze-lines)))
        max-y            (dec (count maze-lines))
        outward-portal-x #{1 (dec max-x)}
        outward-portal-y #{1 (dec max-y)}
        outward-portal?  (fn [[entrance _]]
                           (and (indexed? entrance)
                                (or (outward-portal-x (first entrance))
                                    (outward-portal-y (second entrance)))))
        inward-portal?   (fn [[entrance _]]
                           (and (indexed? entrance)
                                (not (or (outward-portal-x (first entrance))
                                         (outward-portal-y (second entrance))))))]
    (merge maze
           {:outward-portals (into {} (filter outward-portal? (:portals maze)))
            :inward-portals  (into {} (filter inward-portal? (:portals maze)))
            :level           0})))

(declare visit-flat)

(defn try-direction-flat
  "Checks what lies in a particular direction from the current
  coordinates, ignoring portals, and proceeds accordingly. Returns
  with an updated state reflecting the best outcome that can be
  obtained in that direction."
  [{:keys [steps path visited goal portals]
    [x y] :pos
    :as   state}
   direction]
  (let [[x y] (position-after-move x y direction)]
    (cond
      (= [x y] goal)
      state  ; We found a solution!

      (and (path [x y]) (not (visited [x y])))
      (visit-flat (assoc state :pos [x y])) ; A move to a new square, continue solving from here.

      :else ; We can't move in that direction
      (assoc state :steps Long/MAX_VALUE))))

(defn visit-flat
  "Recursive shortest path solver ignoring portals."
  [{[x y] :pos
    :as   state}]
  (reduce best-result
          (map (partial try-direction-flat
                        (-> state
                            (update :steps inc)
                            (update :visited conj [x y])))
               [:north :south :east :west])))

(defn flat-distance
 "Finds the number of steps in the best path, without using portals,
 between the specified points in the maze, which may also be `:start`
 to mean the starting point, and `:exit` to mean the goal at level 0."
 [map start end]
 (let [state (merge map
                    {:pos (if (= start :start)
                            (:start map)
                            (traverse-portals map start))}
                    (when (not= end :exit)
                      {:goal end}))]
   (:steps (visit-flat state))))

(defn build-portal-network
  "Calculates the best paths (if any exist) between each pair of
  portals, as well as between the start and inward portals, and each
  portal outward portal and the exit."
  [maze]
  (let [portals-by-entrance (filter (fn [[k _]] (indexed? k)) (:portals maze))
        portal-point-sets   (map (fn [[end-1 end-2]] (set (concat end-1 end-2)))
                                 (map second (filter (fn [[k _]] (string? k)) (:portals maze))))]
    (reduce (fn [routes [start end]]
              (let [distance (flat-distance maze start end)]
                (if (< distance Long/MAX_VALUE)
                  (assoc routes [start end] distance)
                  routes)))
            {}
            (concat (filter identity
                            (for [[start-entrance start-exit] portals-by-entrance
                                  [end-entrance _]            portals-by-entrance]
                              (let [portal-points (first (filter (fn [points] (points start-entrance))
                                                                 portal-point-sets))]
                                (when (not (portal-points end-entrance))
                                  [start-exit end-entrance]))))
                    (for [[end-entrance _] (:inward-portals maze)]
                      [:start end-entrance])
                    (for [[_ start-exit] (:outward-portals maze)]
                      [start-exit :exit])))))

(declare visit-portals)

(defn try-portal
  "Checks the best result we can obtain when we try to go through a
  particular portal entrance."
  [{:keys [pos steps visited portals outward-portals routes start goal]
    :as   state}
   entrance
   best-so-far]
  #_(println "try-portal" pos entrance steps (:steps best-so-far) (:level state) visited)
  (let [exit     (portals entrance)
        from     (if (= pos start) :start pos)         ; Handle the special cases of the maze start
        to       (if (= entrance goal) :exit entrance) ; and exit.
        distance (routes [from to])
        steps    (+ steps (or distance 0))]
    ;; I thought we would have a problematic loop if we ever revisited the same portal, but that stops actual
    ;; valid solutions from being found, so I need to find better logic for this. Same portal at same level maybe?
    ;; Actually, I probably have to switch to a breadth-first-search.
    (if (and distance #_(not (visited entrance))  ; We can get to this portal and have not yet used it.
             (< steps 10000)  ; Haven't reached distance limit, TODO fix when using breadth-first search
             (< steps (:steps best-so-far))) ; And it won't take more steps than current best solution.
      (visit-portals (-> state
                         (update :visited conj entrance)
                         (update :steps + distance)
                         (update :level + (if (outward-portals entrance) -1 1))
                         (assoc :pos exit))
                     best-so-far)
      (assoc state :steps Long/MAX_VALUE))))  ; Indicate that this way does not lead to a better solution.

(def no-solution
  "A vestigial state map that indicates no solution has been found,
  useful for a base case when updating solutions with best-solution."
  {:steps Long/MAX_VALUE})

(defn portals-available
  "Returns a list of the coordinates of the entrances to portals that
  can be used on the current level, making sure we try the outer ones
  first to avoid infinite regress."
  [{:keys [inward-portals outward-portals level]}]
  (concat (when (pos? level) (keys outward-portals)) (keys inward-portals)))

(defn visit-portals
  "Recursive shortest path maze solver working at the level of portal
  entrances and exits, and managing the rules of the levels on which
  the portals and exit are available. Takes the starting state from
  which to attempt a solution, and the best solution that has been
  found so far, to help short-circuit pointless paths."
  [{:keys [pos steps level]
    :as   state}
   best-so-far]
  (println "visit-portals" pos steps level)
  (let [exit-attempt (if (zero? level) (visit-flat state) no-solution)]
    (loop [best-so-far (best-result best-so-far exit-attempt)
           remaining   (portals-available state)]
      (if-let [portal (first remaining)]
        (recur (best-result best-so-far (try-portal state portal best-so-far))
               (rest remaining))
        best-so-far))))

(defn solve-2
  [maze]
  (let [state (visit-portals (merge maze {:routes (build-portal-network maze)
                                          :pos    (:start maze)})
                             no-solution)
        steps (:steps state)]
    (if (= steps Long/MAX_VALUE)
      :failed
      steps)))

(def part-1-maze
  "The maze for part 1 of the problem."
  "                               Y       P           J       L     Y N     Q   H
                               I       O           J       H     Y S     G   M
  #############################.#######.###########.#######.#####.#.#####.###.###################################
  #.....................#.........#...#.....#.......#.......#.#.......#.....#.........#.#.........#.#.#...#.#...#
  #.#.#.###.###.###.###.#.#.###.###.#.###.###.#######.#.###.#.###.###.#.###.#####.#.###.#.#########.#.###.#.#.###
  #.#.#.#.#.#.#.#...#...#.#.#.#.....#.#.....#.......#.#.#.#...#.....#.#.#.#.#.#...#.............#.#.#...........#
  #####.#.#.#.###.#.###.#####.#.#####.###.#.#####.#.#.###.#########.#####.#.#.###.###############.#.#.#.###.###.#
  #...#...#.#.#...#.#.#.......#.#.....#...#...#...#.#.....#.......#...#.........#...........#...#.....#.#...#.#.#
  ###.#.#####.#######.#.#.###.###.#.#######.###.#####.#########.###.#####.#.#######.#.#########.#.#.#######.#.###
  #.....#.#.#.....#.....#...#.....#.#.#.....#.....#.......#.....#...#...#.#.#.#.#...#.#.#...#...#.#...#...#.....#
  ###.#.#.#.#####.###.###.#####.#####.###.###.#.#.###.#.#.#.#.#.#.###.#.#.###.#.###.#.#.###.#.#.#.#######.#.#####
  #...#.#...#...#.#...#...#.......#.....#.#.#.#.#...#.#.#.#.#.#.....#.#.........#...#.#.....#.#.....#...#.#.#...#
  #.#######.#.###.###.###.#######.#.#.#.#.#.#.###.###.###.#.#.#.#####.#############.###.#######.#######.#.#####.#
  #.#...#...........#.#...#.........#.#.#...#.#.#.#.....#.#.#.#...#.........#...#.#.......#...........#...#.....#
  #.#.#####.#.###.#############.#.###.#####.#.#.#######.#####.#####.#.###.#.#.###.#.###.###.#############.#####.#
  #.#...#.#.#.#.......#...#.....#.#.....#.#.#.......#.#...#...#...#.#...#.#.#...#.#...#...........#.#.#.......#.#
  ###.#.#.#######.###.###.###########.#.#.#.###.#####.#.###.#.#.#####.#.###.#.###.#.#####.#.#.###.#.#.###.#####.#
  #.#.#.#.....#.#.#.#.#...............#.#...#...#...#.....#.#.....#.#.#.#.......#.....#...#.#...#.....#.#.......#
  #.###.#.#####.###.#.#.#######.#####.#.###.#.#####.#.#######.#####.#.###.#.#####.#######.#.###.#.#.###.#.#.#.#.#
  #...#...#.#.#...#...#.#.#.#.#...#.#.#.#.#.#.....#.......#.....#.#...#...#...#.........#.#...#.#.#.#.....#.#.#.#
  #.#####.#.#.###.###.###.#.#.#####.#.###.#.###.#######.###.#.###.#########.#####.#######################.#.###.#
  #.#.........#.#.#.....#...........#...#...#.#...#.#.#...#.#.#...#.........#.....#.#.#.#...#.....#...#...#.#.#.#
  #.#######.###.#.###.#.###########.#.###.###.#.#.#.#.###.###.#.#.#########.#####.#.#.#.###.###.###.#####.###.###
  #.#...#...#.....#...#...#.#.#.......#.....#...#.#.......#...#.#.#.......#...#.....................#.#...#.#.#.#
  #.###.###.#.#########.###.#.###.#.#.#####.#.#######.###.###.#.#######.###.#######.#.###.###.#.###.#.###.#.#.#.#
  #.#...#...#.#.#...#.....#.#...#.#.#.#.....#.....#.....#.#.......#.#...#.....#.#...#...#.#.#.#.#...#...#.......#
  #.###.#.###.#.#.###.###.#.###.###.###.###.#.###.#####.#.#.###.#.#.###.#.#.###.#.#########.#########.#.###.#####
  #.....#.#.#.....#.#.#...............#...#.#.#...#.....#.#.#...#...#.....#...#.......#.#.#.#.......#.#...#...#.#
  #####.#.#.#.#####.#####.#########.#####.###.#######.#######.#.#########.#####.#######.#.#.###.#######.#.#.#.#.#
  #...#...#.#.......#...#.#.#      E     Y   V       U       W L         Y     J      #...#.#...#...#.#.#.#.#.#.#
  ###.#.###.#.###.#.###.###.#      R     I   B       H       T D         Y     R      ###.#.#.###.###.#.###.###.#
  #.......#.....#.#.#...#.#.#                                                         #.............#.....#...#.#
  ###.###.###.#########.#.#.#                                                         #####.###.#.###.#####.###.#
  #.#.#.#.#.#.#.#.....#.....#                                                         #...#.#...#.#...#...#...#.#
  #.#.#.###.#.#.#####.#####.#                                                         #.###.#.#######.#.###.###.#
  #.#...#...#.#...#...#.....#                                                       JJ..#...#...#.#.....#...#.#..KB
  #.###.###.#.###.#.#.#.#.#.#                                                         #.#.#####.#.###.###.###.#.#
  #...#.#.#.........#...#.#..FR                                                       #.....#...................#
  #.###.#.#######.#####.#.#.#                                                         ###########################
RX........#.....#.....#.#.#.#                                                       PO..#.....#...............#..YB
  ###.###.###.#####.#########                                                         #.#.###.###.#.#.#.#####.#.#
  #.....#.............#.....#                                                         #.....#...#.#.#.#.#.......#
  ###.###.###.#.#.#######.###                                                         #.#######.#.#.#.#########.#
PU..#.#.....#.#.#...#...#.#.#                                                         #...#.....#.#.#.....#...#.#
  #.#.###.#######.#####.#.#.#                                                         #####.#####.###.###.###.#.#
  #.#.#.#...#.#...#...#......KB                                                       #.#.#.........#...#...#...#
  #.###.#####.#####.#####.###                                                         #.#.#####.#################
  #.#.#.......#...#.#.#.#...#                                                       YB......#.....#...#.......#.#
  #.#.#.#.#.###.###.#.#.###.#                                                         #.#.#.#########.#.#.#.###.#
  #.....#.#.................#                                                         #.#.#.#.......#.#.#.#.....#
  ###################.#######                                                         ###.#####.#####.#########.#
  #.#.......#.......#...#.#.#                                                         #.#...#.....#.#...#.......#
  #.#.#####.#.###.#.#####.#.#                                                         #.###.###.#.#.#.#######.#.#
  #.....#...#.#.#.#.#.#......BI                                                       #.#.#.....#.............#..LD
  #.#######.#.#.###.#.#####.#                                                         #.#.#####.#################
  #.....#.......#.....#...#.#                                                         #.......#.#...#...#...#...#
  #.###############.###.#.#.#                                                         #.###.#.###.#.#.###.#.#.#.#
YT......#.......#.......#...#                                                         #.#...#...#.#.......#...#..BB
  #####.#.###.###############                                                         #.#####.###.#.###.###.#####
  #...#.#.#.................#                                                       JN....#...#.#.#.#.#...#.#...#
  #.#.#####.#######.#.#.###.#                                                         #######.#.#.#.#.#########.#
  #.#.#.....#.#...#.#.#...#.#                                                         #.#.#.......#.#...#.......#
  #.#.#.###.#.#####.#.#####.#                                                         #.#.###########.#.#######.#
  #.#.#.#.#.....#.#.#.#.#...#                                                         #.....#.....#...#.#.#.....#
  #.#.###.#####.#.#####.###.#                                                         #.#.###.#.#.###.#.#.#.#.#.#
WT..#...........#.#...#.#.#..SG                                                     PU..#.#...#.#.....#.#...#.#..JN
  ###############.#.###.#.###                                                         #.#####.###.#.#.#######.#.#
  #...#.#.............#.....#                                                         #.......#...#.#.....#...#.#
  #.#.#.#.#.#######.#.###.#.#                                                         #.###############.###.#.#.#
  #.#...#.#...#.....#.#...#.#                                                         #...#...#.#.#...#.....#.#.#
  #.#.###.#######.#####.###.#                                                         #####.###.#.###.###########
VB..#...#...#.........#.#.#..LH                                                       #...#.....................#
  #####.#.#####.###.#.#.#.#.#                                                         #.#.#####.###.#.###.#####.#
  #.#.#.......#.#...#.....#.#                                                       NS..#.#.#.#.#.#.#.#.#.#...#.#
  #.#.#####.#.#.#.#.#########                                                         ###.#.#.#.#.#.#.#.#.#.#####
  #...#.#.#.#.#.#.#...#...#.#                                                         #.......#...#.#.#.#...#....KU
  #.#.#.#.###.#########.###.#                                                         #.#####.#.###.###.###.#.###
AA..#.......#.#.#.....#...#..DU                                                       #.#...#...#...#.#...#.....#
  #.###.#######.#####.#.#.#.#                                                         ###.###########.#.#########
  #.#...#.....#.#...#...#.#.#                                                         #.......................#.#
  #.#.###.#.###.###.#.###.#.#                                                         #.#.#.#.#.#.#.#.#.#####.#.#
ER..#.....#...........#.#...#                                                       QG..#.#.#.#.#.#.#.#.#.......#
  ###.#.#.###.###.#.#.#.#.#.#                                                         #.###.#.#######.#####.###.#
  #...#.#.#...#...#.#...#.#.#                                                         #.#...#.#.#.......#...#...#
  #.#.###.#.#####.#########.#                                                         #.#.#.#.#.#.###.###.#####.#
  #.#...#.#...#.......#.....#                                                         #.#.#.#.#...#.....#.#.#....DU
  ###.#.###.#.###.#.#####.#.#        T         Y       H   R       K         B        ###.#####.#####.###.#.###.#
  #.#.#...#.#.#.#.#.#.....#.#        S         T       M   X       U         B        #...#.#.#.#.......#.#.....#
  #.#.#.#####.#.#.#####.#############.#########.#######.###.#######.#########.###########.#.#.#####.#.#.#.###.###
  #...#...#.....#...#.#.#.....#.#.......#.#.#.#.....#.....#.#.....#.#...#.......#...#...#...#.......#.#.#.#.....#
  ###.#######.#####.#.#######.#.###.#####.#.#.###.###.#.###.#.#.###.#.#.#####.#####.#.###.###.#.###.#########.#.#
  #.........#.#.......#.#.............#.....#.....#...#.#.....#.#.....#...#.....#.....#.#.#.#.#.#.#.......#...#.#
  #.###.#####.#######.#.###.###.#.#.#.#.#.#####.###.#####.###.#.#######.#.#.###.#.###.#.###.#.###.#.###.###.#.###
  #...#...#.......#.......#.#.#.#.#.#...#.#.#.....#.....#.#...#...#...#.#.#...#.....#.......#.#.....#.....#.#...#
  #.###.#####.#.###.###.#####.#.#.#.#.#.#.#.###.#####.###########.#.#.###.#######.###.#.#####.#.###.###.###.#####
  #.#...#.....#.#.#.#...#.#.#...#.#.#.#.#...#...#...........#.....#.#.......#...#...#.#.#.....#.#.....#.#.......#
  #.#####.#####.#.#######.#.#.#.#.#######.###.#.#.#####.###.#####.#.#####.#.###.#.###.#####.#.###.#.#######.#.#.#
  #.#.....#.......#.#.........#.#.#...#.....#.#.#...#.....#.#.....#.#.#...#.#.....#.#.#...#.#.#...#.#.#.....#.#.#
  #######.#.#.#.#.#.#########.#######.###.###.###.#####.#.###.###.#.#.#.#.###.###.#.###.###.#######.#.###.#.#.###
  #.......#.#.#.#.#...#.#.#...#.#.........#.....#...#...#...#.#...#...#.#...#...#.....#...#.#.#.#.....#...#.#...#
  #####.###.#.###.#.###.#.#####.#####.###.#.#.###.#######.#.#.###.#.###.#.#######.#####.#####.#.###########.#####
  #.......#.#.#...#.......#.#.......#.#.#.#.#...#.#...#...#.#.#...#...#.#.#.#.......................#.#.#.......#
  ###.#.#########.#######.#.#####.#.###.#.#.#.#####.#####.#######.#.#######.#####.###.#####.###.#.#.#.#.###.#.#.#
  #...#.#.........#.#...#.#.......#.#.....#.#.#.....#...#.#.#.....#...#.#.#.#...#.#.#.#.#...#...#.#.....#...#.#.#
  ###.###.###.#####.###.#.#########.#####.#.###.###.###.#.#.###.###.###.#.#.#.###.#.###.#######.#############.###
  #.....#.#.....#.#.................#.#...#.....#...#.#.....#.....#.#...#.#...#...............#.....#.#.........#
  #.###.#.#######.#.#.###.#####.###.#.###.###.#######.###.#.#####.#.#.###.#.#.#.#.###.###.#####.#.#.#.#######.###
  #.#...#.#...#.....#.#...#.#...#.#.........#.....#...#...#.#.#...#.......#.#.#.#.#.#...#.#.#.#.#.#.........#...#
  #.#####.#.###.#.###.#.###.#####.###.#####.#.###.###.###.###.#.#########.#.#.#.#.#.#.#####.#.###.#.#.###.#####.#
  #.#.#...#...#.#.#...#.#.#.#...#...#.....#.#...#...#...#.#.#...#.#.....#.#.#...#...#.#.#.#...#...#.#.#.......#.#
  #.#.#.#.#.#.###########.#.###.###.###.#####.#######.#.#.#.#.###.#.###.#.#.###########.#.#.###.#.#.#.###.#.#.###
  #.#...#.#.#.#.#.....#.........#.#.#...#.....#.#.....#...#.....#.#.#.#...#.#...#.#.#...#.....#.#.#.#...#.#.#...#
  #.###.###.#.#.###.###.###.###.#.#.###.###.###.###.#######.#####.#.#.#.#.#.#.#.#.#.###.#.#.#.#####.#.#.#.#.###.#
  #.#...#...#...........#.....#.........#.........#.......#.....#...#.#.#.#...#...........#.#...#...#.#.#.#.#...#
  ###################################.#####.#####.#######.#####.###.#############.###############################
                                     J     Z     F       S     T   U             B
                                     R     Z     R       G     S   H             I                                 ")

(defn part-1
  "Solve part 1."
  []
  (solve (read-maze part-1-maze)))

(defn part-2
  []
  (solve-2 (read-maze-2 part-1-maze)))
