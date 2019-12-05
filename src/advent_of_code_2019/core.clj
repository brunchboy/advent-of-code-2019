(ns advent-of-code-2019.core)

;;; Day 1

(def module-masses
  "The masses of all the modules that need to be launched."
  [113481
   140620
   123826
   86474
   71091
   126880
   103784
   140154
   124024
   54281
   80810
   109441
   68828
   144207
   99151
   136876
   99398
   138555
   118619
   133215
   139302
   137780
   136649
   83358
   63027
   75067
   73974
   90158
   94691
   86847
   61466
   81184
   86043
   119923
   116576
   131380
   102136
   143364
   124421
   123141
   138131
   73274
   84598
   61410
   67240
   136186
   63878
   135804
   73599
   84526
   116178
   114587
   58606
   79162
   124031
   120329
   61270
   89887
   54859
   67618
   96669
   56796
   55725
   96105
   68833
   52417
   72249
   53930
   139995
   86217
   131618
   137145
   54944
   76456
   82141
   69754
   102656
   57461
   108747
   79510
   105715
   98046
   116903
   139339
   127451
   135374
   88468
   69524
   76112
   110928
   99160
   137229
   121433
   65951
   56267
   117209
   61358
   73659
   69633
   149274])

(defn fuel
  "Calculate fuel requred to launch a module of the given mass."
  [mass]
  (-> mass
      (/ 3)
      int
      (- 2)))

(defn answer
  "Calculate the sum of fuel requirements for launching all modules."
  []
  (apply + (map fuel module-masses)))

(defn fuel-recursive
  "Calculate the fuel required to launch a module of the given mass,
  taking in account the mass of the fuel itself, recursively, until
  the additional amount is no longer positive."
  [mass]
  (loop [base (fuel mass)
         added (fuel base)]
    (if (pos? added)
      (recur (+ base added) (fuel added))
      base)))

(defn answer-2
  "Calculate the sum of fuel requirements for launching all modules,
  including the fuel."
  []
  (apply + (map fuel-recursive module-masses)))

(def program
  "The opcodes and values for the intcode interpreter of the second problem."
  [1,12,2,3,
   1,1,2,3,
   1,3,4,3,
   1,5,0,3,
   2,10,1,19,
   1,6,19,23,
   1,23,13,27,
   2,6,27,31,
   1,5,31,35,
   2,10,35,39,
   1,6,39,43,
   1,13,43,47,
   2,47,6,51,
   1,51,5,55,
   1,55,6,59,
   2,59,10,63,
   1,63,6,67,
   2,67,10,71,
   1,71,9,75,
   2,75,10,79,
   1,79,5,83,
   2,10,83,87,
   1,87,6,91,
   2,9,91,95,
   1,95,5,99,
   1,5,99,103,
   1,103,10,107,
   1,9,107,111,
   1,6,111,115,
   1,115,5,119,
   1,10,119,123,
   2,6,123,127,
   2,127,6,131,
   1,131,2,135,
   1,10,135,0,
   99,
   2,0,14,0])



;;; Day 2

(defn intcode
  "Run the intcode interpreter on the input data."
  ([]
   (intcode program))
  ([data]
   (loop [i      0
          memory data]
     (let [opcode (nth memory i)
           op1    (nth memory (inc i))
           op2    (nth memory (+ i 2))
           dest   (nth memory (+ i 3))]
       #_(println "i:" i "opcode:" opcode "op1:" op1 "op2" op2 "dest:" dest)
       (case opcode
         1  (recur (+ i 4)
                   (assoc memory dest (+ (nth memory op1) (nth memory op2))))
         2  (recur (+ i 4)
                   (assoc memory dest (* (nth memory op1) (nth memory op2))))
         99 memory
         (println "Unknown opcode:" opcode))))))

;; Part 2

(defn brutecode
  "Hunt for input values that yield 19690720 in position 0"
  []
  (doseq [noun (range 100)
          verb (range 100)]
    (let [data   (-> program
                     (assoc 1 noun)
                     (assoc 2 verb))
          output (first (intcode data))]
      (when (= output 19690720)
        (println "Match! noun:" noun "verb:" verb "answer:" (+ (* 100 noun) verb)))))
  nil)



;;; Day 3

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


;;; Day 4

(defn valid?
  "Checks whether a sequence of digits meets the password rules for day
  4."
  [password]
  (let [digits (map #(Integer/valueOf (str %)) (str password))]
    (and (some #(apply = %) (partition 2 1 digits))
         (apply <= digits))))

;; Example use: (count (filter valid? (range 271973 785962)))

(defn valid-2?
  "Checks whether a sequence of digits meets the part 2 password rules
  for day 4."
  [password]
  (let [digits (map #(Integer/valueOf (str %)) (str password))]
    (and (some #(= 2 %) (vals (frequencies digits)))
         (apply <= digits))))


;;; Day 5

(def program-5
  "The intcode program data for day 5 part 1"
  [3,225,1,225,6,6,1100,1,238,225,104,0,1101,61,45,225,102,94,66,224,101,-3854,224,224,4,224,
   102,8,223,223,1001,224,7,224,1,223,224,223,1101,31,30,225,1102,39,44,224,1001,224,-1716,224,
   4,224,102,8,223,223,1001,224,7,224,1,224,223,223,1101,92,41,225,101,90,40,224,1001,224,-120,224,
   4,224,102,8,223,223,1001,224,1,224,1,223,224,223,1101,51,78,224,101,-129,224,224,4,224,1002,223,
   8,223,1001,224,6,224,1,224,223,223,1,170,13,224,101,-140,224,224,4,224,102,8,223,223,1001,224,
   4,224,1,223,224,223,1101,14,58,225,1102,58,29,225,1102,68,70,225,1002,217,87,224,101,-783,224,224,
   4,224,102,8,223,223,101,2,224,224,1,224,223,223,1101,19,79,225,1001,135,42,224,1001,224,-56,224,
   4,224,102,8,223,223,1001,224,6,224,1,224,223,223,2,139,144,224,1001,224,-4060,224,4,224,
   102,8,223,223,101,1,224,224,1,223,224,223,1102,9,51,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,
   1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,
   1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,
   1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,
   1105,1,99999,1008,677,226,224,102,2,223,223,1006,224,329,101,1,223,223,108,677,677,224,
   102,2,223,223,1005,224,344,101,1,223,223,107,677,677,224,1002,223,2,223,1005,224,359,
   101,1,223,223,1107,226,677,224,1002,223,2,223,1005,224,374,1001,223,1,223,1008,677,677,224,
   102,2,223,223,1006,224,389,1001,223,1,223,1007,677,677,224,1002,223,2,223,1006,224,404,
   1001,223,1,223,8,677,226,224,102,2,223,223,1005,224,419,1001,223,1,223,8,226,226,224,
   102,2,223,223,1006,224,434,101,1,223,223,1107,226,226,224,1002,223,2,223,1006,224,449,
   101,1,223,223,1107,677,226,224,102,2,223,223,1005,224,464,101,1,223,223,1108,226,226,224,
   102,2,223,223,1006,224,479,1001,223,1,223,7,677,677,224,1002,223,2,223,1006,224,494,
   101,1,223,223,7,677,226,224,102,2,223,223,1005,224,509,101,1,223,223,1108,226,677,224,
   1002,223,2,223,1006,224,524,101,1,223,223,8,226,677,224,1002,223,2,223,1005,224,539,101,1,223,223,
   1007,226,226,224,102,2,223,223,1006,224,554,1001,223,1,223,108,226,226,224,1002,223,2,223,
   1006,224,569,1001,223,1,223,1108,677,226,224,102,2,223,223,1005,224,584,101,1,223,223,
   108,226,677,224,102,2,223,223,1005,224,599,101,1,223,223,1007,226,677,224,102,2,223,223,
   1006,224,614,1001,223,1,223,1008,226,226,224,1002,223,2,223,1006,224,629,1001,223,1,223,
   107,226,226,224,1002,223,2,223,1006,224,644,101,1,223,223,7,226,677,224,102,2,223,223,
   1005,224,659,1001,223,1,223,107,677,226,224,102,2,223,223,1005,224,674,1001,223,1,223,4,223,99,226])

(defn intcode-5
  "Run the day 5 intcode interpreter on the input data."
  ([]
   (intcode program-5))
  ([data]
   (loop [i      0
          memory data]
     (let [load-relative   (fn [offset] (nth memory (+ i offset)))
           instruction     (load-relative 0)
           opcode          (rem instruction 100)
           modes           (clojure.string/reverse (str (quot instruction 100)))
           resolve-operand (fn [index]
                             (let [value (load-relative (inc index))
                                   mode  (nth modes index \0)]
                               (case mode
                                 \0 (nth memory value)
                                 \1 value)))]
       #_(println "i:" i "opcode:" opcode "op1:" op1 "op2" op2 "dest:" dest)
       (case opcode
         1  (recur (+ i 4)  ; Add
                   (assoc memory (load-relative 3) (+ (resolve-operand 0) (resolve-operand 1))))
         2  (recur (+ i 4)  ; Multiply
                   (assoc memory (load-relative 3) (* (resolve-operand 0) (resolve-operand 1))))
         3  (recur (+ i 2)  ; Read
                   (assoc memory (load-relative 1) (read)))
         4  (recur (+ i 2)  ; Print
                   (do
                     (println (resolve-operand 0))
                     memory))
         5  (recur (if (zero? (resolve-operand 0))  ; Jump if true
                     (+ i 3)
                     (resolve-operand 1))
                   memory)
         6  (recur (if (zero? (resolve-operand 0))  ; Jump if false
                     (resolve-operand 1)
                     (+ i 3))
                   memory)
         7  (recur (+ i 4)  ; Less than
                   (assoc memory (load-relative 3)
                          (if (< (resolve-operand 0) (resolve-operand 1)) 1 0)))
         8  (recur (+ i 4)  ; Equals
                   (assoc memory (load-relative 3)
                          (if (= (resolve-operand 0) (resolve-operand 1)) 1 0)))
         99 memory
         (println "Unknown opcode:" opcode))))
   nil))
