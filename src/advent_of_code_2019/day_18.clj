(ns advent-of-code-2019.day-18
  "Solutions to the Day 18 problems."
  (:require [clojure.math.combinatorics :as combo]
            [clojure.repl :refer :all]))

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

(def sample-state
  "Shows the structure of the low-level maze traversal state map, used
  when mapping out the distances and doorways between the starting
  point and keys, and between pairs of keys. For the first pass we
  ignore the closed state of doors and simply record the keys and
  doors passed over on the shortest path from the starting point to
  the ending point. If there are ties, we record the doors and keys
  traversed on each. If we already have a key when we encounter a
  door, the door does not get recorded because it is not an obstacle
  on that path."
  {:walls          #{[0 0]     ; Identifies the locations of all walls present in the maze.
                     [2 2]}
   :keys           {[1 1] "b" ; Identifies the locations of all keys present in the maze.
                    [7 1] "a"}
   :doors          {[3 1] "a"} ; Identifies the locations of all doors present in the maze.
   :pos            [5 1]       ; Identifies the current cell being traversed.
   :steps          0           ; Steps traversed so far / steps required for solution.
   :keys-found     #{"a"}      ; Keys that were passed over in this traversal.
   :doors-blocking #{"b"}      ; The doors we need keys for before this path works.
   :visited        #{[5 1]}    ; Cells that have already been visited.
   ;; The purpose of the first, maze-walking phase is to build up the following information.
   :routes         {#{"@" "a"}                     ; Records the steps of each best path from the start to a key,
                    [1234 #{{:doors-blocking #{"b"} ; and the obstacles and keys found along it.
                             :keys-found     #{}}}] ; There may be multiple doors/keys alternatives.
                    #{"@" "b"}                   ; And another route from the start to a key...
                    [567 #{{:doors-blocking #{}}}]
                    #{"a" "b"}                      ; Best route from one key to another in either direction.
                    [312 #{{:doors-blocking #{}
                            :keys-found     #{}}}]}})

(defn current-route
  "Extracts the currently best-known route from start to target from
  the state. If none is found, return a route that will be beaten by
  any real one."
  [start target state]
  (get-in state [:routes #{start target}] [Long/MAX_VALUE #{{:doors-blocking #{}
                                                             :keys-found        #{}}}]))

(defn add-route
  "Called when we have found and are returning a route from start to
  target. If it is better than any other such route we have seen,
  install it as the current choice. If it has the same number of steps
  as our current best route, add the keys and doors we saw as an
  alternate choice to the other equal-step routes."
  [start target
   {:keys [steps keys-found doors-blocking] :as state}]
  (let [current      (current-route start target state)
        current-best (first current)
        found-info   {:keys-found     keys-found
                      :doors-blocking doors-blocking}]
    (cond (< steps current-best) ; We found a new best route, so replace the old one.
          (-> state
              (assoc :steps steps)
              (assoc-in [:routes #{start target}] [steps #{found-info}]))

          (= steps current-best) ; We found a tie, add our keys and doors to the existing set.
          (update-in state [:routes #{start target} 1] conj found-info)

          :else ; Our old route was better, leave it alone.
          state)))

(defn merge-routes
  "Called when we have found states representing two different routes
  from start to target in different directions. Picks the best. If
  they are both the same length, combines their sets of keys/doors
  sets."
  [start target state-1 state-2]
  (let [route-1 (current-route start target state-1)
        steps-1 (:steps state-1)
        route-2 (current-route start target state-2)
        steps-2 (:steps state-2)]
    (cond (< steps-2 steps-1)
          state-2

          (= steps-2 steps-1)
          (update-in state-1 [:routes #{start target} 1] clojure.set/union (second route-2))

          :else
          state-1)))

(defn try-direction
  "Checks what lies in a particular direction from the current
  coordinates, given the key we started at (or `@` if it is the maze
  starting position), the key we are trying to find and the current
  state. Returns with an updated state reflecting the best outcome
  that can be obtained in that direction."
  [start target
   {:keys [walls doors steps keys-found doors-blocking visited routes]
    [x y] :pos
    :as   state}
   direction]
  #_(println "trying" x y steps direction)
  (let [[x y]      (position-after-move x y direction)
        key-found  ((:keys state) [x y])
        door-found (doors [x y])]

    (cond
      (or ((:walls state) [x y]) ((:visited state) [x y]))
      (assoc state :steps Long/MAX_VALUE)

      key-found
      (if (= key-found target)
        (add-route start target state) ; Found a route to the target, record it if best/equal and return.
        (visit start target (assoc state :pos [x y] :keys-found (conj keys-found key-found)))) ; Keep looking.

      (and door-found #(not (keys-found door-found))) ; Found a door that could block us.
      (visit start target (assoc state :pos [x y] :doors-blocking (conj doors-blocking door-found)))

      :else
      (visit start target (assoc state :pos [x y])))))  ; An ordinary move, continue solving from here.

(defn visit
  "Recursive shortest path maze solver, given the key we started at (or
  `@` if it is the maze starting position), the key we are trying to
  find, and the current state. Returns a state whose routes reflect
  the smallest number of steps between those points, along with the
  sets of keys found along the way, and doors for which other keys are
  needed before they can be traversed. (There may be multiple
  keys/doors set pairs in the route because there may be more than one
  path with that number of steps.) No route will be added (and
  `:steps` will be `Long/MAX_VALUE`) if no path was found."
  [start target
   {[x y] :pos
    :as   state}]
  #_(println start target x y)
  (reduce (partial merge-routes start target)
          (map (partial try-direction start target
                       (-> state
                           (update :steps inc)
                           (update :visited conj [x y])))
              [:north :south :east :west])))

(defn initial-state-for-map
  "Converts a maze map into the initial state needed by the solver.
  Removes the player entry, sets up the visited set, turns the walls
  entry into a simple set of coordinates, and builds an index from key
  name to its coordinate to make it easy to build the route map."
  [maze]
  (-> maze
      (merge {:steps          0                             ; Counts steps as we explore the maze.
              :pos             (first (keys (:player maze))) ; Tracks the location we have reached.
              :keys-found     #{}                           ; Keys that we have passed during this traversal.
              :doors-blocking #{}                           ; Doors we need other keys for before this path works.
              :key-index      (clojure.set/map-invert (:keys maze))  ; Make it easy to search from them.
              :visited        #{}})  ; Keep track of places we have been.
      ;; This needs to move to the higher-level solver:
      #_(assoc :keys-found [])  ; Keep track of the keys we have found along this path, in order.
      (update :walls (fn [m] (set (keys m))))))  ; All we need is the set of coordinates.

(defn search-from
  "Sets up a search from the specified key location. Resets the steps
  visited, keys-found and doors-blocking accumulators since this is a
  new search."
  [start state]
  (let [starting-pos (if (= "@" start) (first (keys (:player state))) (get-in state [:key-index start]))]
    (merge state {:pos            starting-pos ; Start from the specified key location.
                  :steps          0            ; This is a new search, start over at zero.
                  :visited        #{}          ; And we haven't walked anywhere yet this time.
                  :keys-found     #{}          ; Keys that we have passed during this traversal.
                  :doors-blocking #{}})))      ; Doors we need other keys for before this path works.

(defn other-key
  "Given a key pair set and one of the keys it contains, returns the
  other."
  [pair k]
  (first (disj pair k)))

(defn find-route
  "Finds the best route from either the start position or a key to
  another key, counting the steps, noting any other keys that can be
  picked up along the way, and any doors that must be unlocked before
  the path can be followed. If there is more than one route with the
  same minimum steps, reports all the distinct sets of keys/doors
  along each of them. `pair` is a set of start location and target
  key (the order does not matter because the route returns the same
  values in either direction, but for calling `visit` in the special
  case where we want to start at the start of the maze, one of the
  elements will be `@` and that will need to be the first argument to
  `visit`."
  [pair state]
  (let [[start target] (if (pair "@")
                         ["@" (other-key pair "@")]
                         (vec pair))]
    (visit start target (search-from start state))))

(defn find-key-routes
  "Accumulates all the best paths from the start to each key, and from
  each key to each other key, with notes about the doors blocking
  each, and extra keys found along the way."
  [maze]
  (reduce (fn [state pair]
            (let [with-new-route (find-route pair state)]
              (if (< (:steps with-new-route) Long/MAX_VALUE)  ; Only include working routes.
                with-new-route
                state)))
          maze
          (map set (combo/combinations (conj (keys (:key-index maze)) "@") 2))))

(defn can-reach-key
  "Given a set of key route path annotations (which include the doors
  blocking each path), and the keys which have not yet been found,
  returns an indication of whether the key can currently be reached."
  [path-set keys-left]
  (some (fn [{:keys [doors-blocking]}]
          (empty? (clojure.set/intersection keys-left doors-blocking)))
        path-set))

(defn accessible-keys
  "Returns all the keys we can get to from our current position without
  opening any new doors. Returns tuples containing the key that can be
  reached and the route (steps required, additional keys encountered,
  and doors in the way, for which we must already have keys)."
  [state keys-left pos]
  (filter (fn [[k]]  ; Ignore the "route" back to the starting point.
            (not= "@" k))
          (map (fn [[pair route]]
                 [(other-key pair pos) route])
               (filter (fn [[pair [_ path-set]]]
                         (when (and (pair pos)
                                    (can-reach-key path-set keys-left))
                           (first (disj pair pos))))
                       (:routes state)))))

(defn solve
  "Sets up the state given the map that was read, and runs the solvers."
  [maze]
  ;; TODO: Change to set up key routes and run a depth first search based on them.
  (let [state (visit "@" "a" (initial-state-for-map maze))
        steps (:steps state)]
    (if (= steps Long/MAX_VALUE)
      :failed
      [steps (:keys-found state)])))

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
