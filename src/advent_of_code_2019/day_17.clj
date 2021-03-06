(ns advent-of-code-2019.day-17
  "Solutions to the Day 17 problems."
  (:require [clojure.repl :refer :all]
            [clojure.math.combinatorics :as combo]
            [advent-of-code-2019.day-9 :refer [intcode]]))

(def program
  "The 'ASCII' intcode program."
  [1,330,331,332,109,4288,1102,1,1182,15,1102,1479,1,24,1002,0,1,570,1006,570,36,1002,571,1,0,1001,570,-1,570,1001,24,1,24,1105,1,18,1008,571,0,571,1001,15,1,15,1008,15,1479,570,1006,570,14,21102,58,1,0,1106,0,786,1006,332,62,99,21101,0,333,1,21101,73,0,0,1106,0,579,1102,1,0,572,1102,0,1,573,3,574,101,1,573,573,1007,574,65,570,1005,570,151,107,67,574,570,1005,570,151,1001,574,-64,574,1002,574,-1,574,1001,572,1,572,1007,572,11,570,1006,570,165,101,1182,572,127,1001,574,0,0,3,574,101,1,573,573,1008,574,10,570,1005,570,189,1008,574,44,570,1006,570,158,1106,0,81,21101,340,0,1,1105,1,177,21102,477,1,1,1106,0,177,21101,514,0,1,21101,0,176,0,1105,1,579,99,21102,184,1,0,1105,1,579,4,574,104,10,99,1007,573,22,570,1006,570,165,1002,572,1,1182,21101,0,375,1,21102,1,211,0,1106,0,579,21101,1182,11,1,21101,0,222,0,1106,0,979,21101,388,0,1,21101,233,0,0,1105,1,579,21101,1182,22,1,21101,0,244,0,1105,1,979,21102,401,1,1,21102,255,1,0,1105,1,579,21101,1182,33,1,21101,0,266,0,1106,0,979,21102,414,1,1,21102,277,1,0,1106,0,579,3,575,1008,575,89,570,1008,575,121,575,1,575,570,575,3,574,1008,574,10,570,1006,570,291,104,10,21101,0,1182,1,21101,0,313,0,1105,1,622,1005,575,327,1101,0,1,575,21102,327,1,0,1105,1,786,4,438,99,0,1,1,6,77,97,105,110,58,10,33,10,69,120,112,101,99,116,101,100,32,102,117,110,99,116,105,111,110,32,110,97,109,101,32,98,117,116,32,103,111,116,58,32,0,12,70,117,110,99,116,105,111,110,32,65,58,10,12,70,117,110,99,116,105,111,110,32,66,58,10,12,70,117,110,99,116,105,111,110,32,67,58,10,23,67,111,110,116,105,110,117,111,117,115,32,118,105,100,101,111,32,102,101,101,100,63,10,0,37,10,69,120,112,101,99,116,101,100,32,82,44,32,76,44,32,111,114,32,100,105,115,116,97,110,99,101,32,98,117,116,32,103,111,116,58,32,36,10,69,120,112,101,99,116,101,100,32,99,111,109,109,97,32,111,114,32,110,101,119,108,105,110,101,32,98,117,116,32,103,111,116,58,32,43,10,68,101,102,105,110,105,116,105,111,110,115,32,109,97,121,32,98,101,32,97,116,32,109,111,115,116,32,50,48,32,99,104,97,114,97,99,116,101,114,115,33,10,94,62,118,60,0,1,0,-1,-1,0,1,0,0,0,0,0,0,1,20,20,0,109,4,2101,0,-3,586,21001,0,0,-1,22101,1,-3,-3,21101,0,0,-2,2208,-2,-1,570,1005,570,617,2201,-3,-2,609,4,0,21201,-2,1,-2,1105,1,597,109,-4,2106,0,0,109,5,1202,-4,1,630,20102,1,0,-2,22101,1,-4,-4,21102,1,0,-3,2208,-3,-2,570,1005,570,781,2201,-4,-3,653,20102,1,0,-1,1208,-1,-4,570,1005,570,709,1208,-1,-5,570,1005,570,734,1207,-1,0,570,1005,570,759,1206,-1,774,1001,578,562,684,1,0,576,576,1001,578,566,692,1,0,577,577,21102,1,702,0,1106,0,786,21201,-1,-1,-1,1105,1,676,1001,578,1,578,1008,578,4,570,1006,570,724,1001,578,-4,578,21101,0,731,0,1105,1,786,1106,0,774,1001,578,-1,578,1008,578,-1,570,1006,570,749,1001,578,4,578,21101,0,756,0,1105,1,786,1105,1,774,21202,-1,-11,1,22101,1182,1,1,21102,1,774,0,1105,1,622,21201,-3,1,-3,1105,1,640,109,-5,2105,1,0,109,7,1005,575,802,21001,576,0,-6,21002,577,1,-5,1106,0,814,21102,0,1,-1,21101,0,0,-5,21102,0,1,-6,20208,-6,576,-2,208,-5,577,570,22002,570,-2,-2,21202,-5,53,-3,22201,-6,-3,-3,22101,1479,-3,-3,1201,-3,0,843,1005,0,863,21202,-2,42,-4,22101,46,-4,-4,1206,-2,924,21102,1,1,-1,1106,0,924,1205,-2,873,21101,35,0,-4,1105,1,924,1201,-3,0,878,1008,0,1,570,1006,570,916,1001,374,1,374,2101,0,-3,895,1102,1,2,0,1202,-3,1,902,1001,438,0,438,2202,-6,-5,570,1,570,374,570,1,570,438,438,1001,578,558,922,20102,1,0,-4,1006,575,959,204,-4,22101,1,-6,-6,1208,-6,53,570,1006,570,814,104,10,22101,1,-5,-5,1208,-5,53,570,1006,570,810,104,10,1206,-1,974,99,1206,-1,974,1101,0,1,575,21102,1,973,0,1106,0,786,99,109,-7,2105,1,0,109,6,21102,0,1,-4,21102,0,1,-3,203,-2,22101,1,-3,-3,21208,-2,82,-1,1205,-1,1030,21208,-2,76,-1,1205,-1,1037,21207,-2,48,-1,1205,-1,1124,22107,57,-2,-1,1205,-1,1124,21201,-2,-48,-2,1105,1,1041,21101,-4,0,-2,1105,1,1041,21102,-5,1,-2,21201,-4,1,-4,21207,-4,11,-1,1206,-1,1138,2201,-5,-4,1059,2101,0,-2,0,203,-2,22101,1,-3,-3,21207,-2,48,-1,1205,-1,1107,22107,57,-2,-1,1205,-1,1107,21201,-2,-48,-2,2201,-5,-4,1090,20102,10,0,-1,22201,-2,-1,-2,2201,-5,-4,1103,2101,0,-2,0,1106,0,1060,21208,-2,10,-1,1205,-1,1162,21208,-2,44,-1,1206,-1,1131,1105,1,989,21101,439,0,1,1105,1,1150,21102,477,1,1,1105,1,1150,21102,514,1,1,21102,1149,1,0,1105,1,579,99,21101,0,1157,0,1105,1,579,204,-2,104,10,99,21207,-3,22,-1,1206,-1,1138,1201,-5,0,1176,1201,-4,0,0,109,-6,2106,0,0,36,7,46,1,5,1,46,1,5,1,46,1,5,1,46,1,5,1,46,1,5,1,46,1,5,1,46,1,5,1,44,9,44,1,1,1,44,9,44,1,5,1,46,1,5,1,46,1,5,1,44,9,44,1,1,1,50,1,1,1,50,1,1,1,50,1,1,11,40,1,11,1,14,9,1,11,5,1,7,9,10,1,7,1,1,1,15,1,7,1,3,1,3,1,10,1,7,1,1,1,15,1,7,1,3,1,3,1,10,1,7,1,1,1,15,1,7,1,3,1,3,1,10,1,7,1,1,1,15,7,1,1,3,1,3,1,10,1,7,1,1,1,21,1,1,1,3,1,3,1,10,11,21,1,1,1,3,7,16,1,23,1,1,1,7,1,1,1,16,7,17,1,1,1,7,11,14,1,17,1,1,1,9,1,7,1,14,1,9,11,9,1,7,1,14,1,9,1,7,1,11,1,7,1,14,1,9,1,7,1,11,1,7,1,14,1,9,1,7,1,11,1,7,1,14,1,9,1,7,11,1,9,14,1,9,1,17,1,24,11,17,1,52,1,46,9,44,1,5,1,46,1,5,1,46,1,5,1,44,9,44,1,1,1,44,9,44,1,5,1,46,1,5,1,46,1,5,1,46,1,5,1,46,1,5,1,46,1,5,1,46,1,5,1,46,7,18])

(defn show-camera-view
  "Run the 'ASCII' program and print the output"
  []
  (doseq [code (intcode program [])]
    (print (char code))))

(defn gather-map
  "Given the output of the camera program, return the matrix of values
  returned."
  [ascii]
  (loop [rows        []
         current-row []
         remaining   ascii]
    (if-let [square (first remaining)]
      (do (print (char square))
          (if (= square 10)
            (recur (conj rows (apply str current-row))
                   []
                   (rest remaining))
            (recur rows
                   (conj current-row (char square))
                   (rest remaining))))
      (conj rows (apply str current-row)))))

(defn get-square
  "Get the character at the specified coordinates in the map."
  [scaffold-map [x y]]
  (nth (nth scaffold-map y) x))

(defn intersection-squares
  "Given a pair of coordinates representing a potential intersection,
  returns all the coordinates that must contain scaffolding for it to
  count as an intersection."
  [x y]
  [[x y] [(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])

(defn intersection-score
  "Checks if the coordinates represent an intersection. If so, returns
  the product of the coordinates, otherwise returns 0."
  [scaffold-map x y]
  (if (every? #{\#} (map (partial get-square scaffold-map) (intersection-squares x y)))
    (* x y)
    0))

(defn find-calibration-parameters
  "Run the 'ASCII` program and collect the map data, then scan it for
  intersections and add up their coordinate products."
  []
  (let [scaffold-map (vec (remove clojure.string/blank? (gather-map (intcode program []))))]
    (apply + (map (fn [[x y]] (intersection-score scaffold-map x y))
                  (for [y (range 1 (dec (count scaffold-map)))
                        x (range 1 (dec (count (nth scaffold-map y))))]
                      [x y])))))

(def full-move-list
  "The full set of moves the vacuum robot needs to take in order to
  traverse the scaffolding."
  "L,10,L,6,R,10,R,6,R,8,R,8,L,6,R,8,L,10,L,6,R,10,L,10,R,8,R,8,L,10,R,6,R,8,R,8,L,6,R,8,L,10,R,8,R,8,L,10,R,6,R,8,R,8,L,6,R,8,L,10,L,6,R,10,L,10,R,8,R,8,L,10,R,6,R,8,R,8,L,6,R,8")

;; It turned out I had a transcription error in my list, an extra R,8
;; six moves in, which made me unable to solve the problem until a
;; friend noticed that. Sigh! Given that fix, visual inspection was
;; enough to come up with the following solution.
(defn part-2
  "Run the vacuum robot with the movement instructions identified by
  studying the map. Print the resulting state and dust count."
  []
  (let [response (intcode (assoc program 0 2)
                          (map long (concat "A,B,A,C,B,C,B,A,C,B\n"
                                            "L,10,L,6,R,10\n"
                                            "R,6,R,8,R,8,L,6,R,8\n"
                                            "L,10,R,8,R,8,L,10\n"
                                            "n\n")))]
    (doseq [code (butlast response)]
      (print (char code)))
    (last response)))



;; These were close to working but could not find any partitions that
;; let me have only three movement functions, due to the transcription
;; mistake mentioned above:

(defn generate-move-partitions
  [moves]
  (filter #(= (count moves) (apply + %))
          (mapcat (partial combo/permuted-combinations (mapcat (partial repeat 8) (range 3 8))) (range 3 20))))

(defn split-moves
  [moves chunks]
  (loop [result #{}
         moves  moves
         chunks chunks]
    (if-let [chunk (first chunks)]
      (recur (conj result (vec (take chunk moves)))
             (drop chunk moves)
             (rest chunks))
      result)))

(defn find-working-moves
  []
  (let [moves      (map (partial clojure.string/join ",")
                        (partition 2 (clojure.string/split full-move-list #",")))
        partitions (generate-move-partitions moves)
        chunked (map (partial split-moves moves) partitions)]
    (println (count chunked) "sets of distinct move patterns.")
    (println (count (first chunked)) "move patterns in the first.")
    #_(filter #(= 3 (count %)) chunked)
    chunked))

;; I couldn't get these working, and went with the more-bounded
;; approach above instead.

(defn all-partitions-from-here
  [moves]
  (when (seq moves)
    (for [i (range 1 (count moves))]
      (let [prefix (list (clojure.string/join "," (take i moves)))]
        (println prefix (drop i moves))
        #_(concat prefix (drop i moves))
        (mapcat (partial concat prefix) (all-partitions-from-here (drop i moves)))))))

(defn all-partitions
  "Generates every possible way to split up the move list, assuming that
  we don't need to break it up down any further than a rotation
  followed by the full amount to move in that direction."
  []
  (let [moves (map (partial clojure.string/join ",")
                   (partition 2 (clojure.string/split full-move-list #",")))]
    (all-partitions-from-here moves)))
