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

(def day-5-orbits "F6J)1YB
6LV)SG3
K7G)GD2
JC1)Y2W
43D)SP2
YQV)JKG
TD4)7SZ
H8T)43D
T1S)H8Y
1BW)H7F
PDV)93Q
2HK)Z93
37L)1FF
35Y)MZH
7NY)DWF
YLS)5B6
N66)QLD
T9K)TMS
JZF)7TC
9QD)YRG
5T2)CYY
DBP)FG7
JVN)N7N
Q78)K9T
6CZ)D66
WD3)LNP
7YB)Y9T
Z3S)115
2PD)RC7
XZS)DZD
PCP)3YG
QYH)3CV
F3M)1SZ
37W)C4L
W1Y)T5F
S9V)TD7
L52)81R
7F1)45V
858)YN9
4JB)RMY
NL4)F8R
V6D)KPP
ZMR)KXH
X7Y)N3G
TRT)W5B
FHH)MNV
725)W9Y
QYG)D7S
LQX)ZQK
KTL)G6X
Z93)GZS
W28)BFC
PR1)9B6
MNX)Y76
YZG)1WK
X6S)H9M
GPY)HLS
7XR)G7Q
5B1)MVF
3GK)XTS
KFK)36G
5B6)PD9
TL4)NXV
265)84G
Z5T)B78
JBB)LG9
GLM)1H1
H6F)P9N
DZP)MFM
ZNL)FHH
QRZ)M8Q
QQ6)1KW
SG3)GPW
D85)QX4
W85)Y98
4SF)R39
7RL)54X
GX8)6QS
MD4)6ZK
5ST)NHM
6GS)9G6
6M7)8FF
GZS)XFJ
NHM)SMF
Y8X)KTL
V8R)9N5
PVX)G41
JP5)TRV
29T)LVJ
B78)9FH
HGW)MXC
D3K)PX5
BMQ)7KP
5FP)CSK
X7T)2JP
TD7)5BW
9BG)2LR
DQS)FKY
XZC)W1J
3LR)DVC
FMK)3WS
Y7C)177
C7X)LSN
HG3)SZM
P9N)KNV
YW9)WL7
MD7)XZC
RC1)S1N
M8Q)LDZ
23V)ZPH
NR2)WYF
VN8)2XR
44R)KH9
BHZ)ZMG
G89)38T
8CC)6VK
7YS)3LL
KZG)V5V
G1B)GNM
KPP)64T
B8P)W9T
2VT)FL6
WWC)Y6F
KHT)GHX
83D)NZK
64T)JD7
VVL)7RB
3WK)1NW
H36)Z4K
8L8)M2Y
7R5)RSS
N8V)V8G
NG4)YKB
SHH)PVX
PN2)2L5
ZRT)J2C
LDZ)73H
CSK)H47
QDB)9W9
2D4)26N
YJT)7YS
DX8)X7Y
MW3)6YY
TLC)KJM
B58)5XY
Q6R)WXJ
P8F)QNY
Y6F)JWT
CRD)FVT
KLW)TMQ
9M1)TWB
NHZ)3CS
9ZL)CRD
R9H)HM1
YD5)TX1
FPR)4P4
GR6)T1S
7NZ)X1G
WJZ)4LY
QCG)D3G
685)PTQ
FZJ)4NP
56S)78G
CC7)ZWF
XBT)7ZQ
BH3)MRP
2W1)P61
QS6)M4N
X2P)Y28
5TG)VW5
DM7)893
X2P)JY1
XKV)WLK
XFJ)QP3
PNH)7GL
DNH)FPN
ZTK)9H9
FFN)M3W
FZR)741
54S)C1Z
S2M)ZNL
7DW)TWY
FKY)RLH
SKG)KLW
ML8)6PR
PS2)FS6
WSV)R4R
MYQ)3JX
ZYP)LRB
96X)Q7Z
7M9)RC1
QKQ)QYH
TPB)YHM
TX1)4PK
X9Q)3RC
ZZ3)ZJT
HF3)HPL
ZPH)G7G
T6N)QG1
ZJT)FVH
J2L)DT4
6LP)HKV
TJJ)83L
CNH)6M7
B55)V2V
1TK)FGT
NLQ)968
37H)F7V
SZM)XKV
5ZF)JL1
2C6)YVD
Y55)H3N
TJZ)R1D
F79)NJT
XC3)QTM
1RX)WMY
L5N)ZTK
N43)TSG
PFC)61H
WD7)WH2
11D)68G
NXV)84Z
NMG)MNX
LRZ)N31
8PQ)MJC
W15)WZ3
V5H)21N
DXX)Z79
L8M)57F
GYD)WQ6
4FH)P7L
W3L)1M2
F7V)Y3L
64K)6LP
GBK)J1M
GNM)BMQ
FMP)H36
865)ZMW
5ZH)931
4P4)2DN
7PT)DM7
F4X)1VY
BSM)JWV
V1J)W15
HGW)SS8
JK5)MD5
Z79)WQZ
1V6)PN2
3Z1)VQR
G2B)333
C5D)F3M
W5B)RBC
3R9)C7X
115)5JK
KMG)9G9
DYR)1QX
GLR)KNJ
C59)W28
V5M)FMP
DGY)LQN
WVY)JLW
376)YJT
LBK)ZCV
JWD)YPX
98D)CWQ
P3H)9ZL
VJG)596
R62)QDK
KC6)5B1
WDF)MN4
4D8)K74
BXF)6RY
FNX)SKG
FPN)2DB
3KH)H87
YFW)FVN
8JS)6J8
C4G)MMV
TD3)FGL
1TX)Z9M
18G)Y55
RCS)ZMR
4F2)CNX
YKP)54S
RWG)4DH
RR7)JQL
N6H)3YV
BLL)PCP
DJP)MJR
7ZK)4HV
9H6)BMX
DT4)GK5
WYF)P59
QPX)NKZ
177)BMY
LHK)4ZZ
HN7)JWD
1TL)RX6
WD5)SNT
R4R)CVL
4JN)DC9
JXW)1RX
VBR)99Q
T53)KLR
5T1)7HH
6MH)3H4
HHN)PHL
BNM)RJ4
7XM)QYG
V9L)TJN
Y79)BSP
LLF)5KJ
VS5)5S9
DJY)KTQ
Q57)37H
HPL)9M1
YWN)YTR
FW4)8Y4
T4F)NHZ
WQ6)61N
8LF)BVP
5MQ)Z5F
KG5)N99
NZK)YNY
8FM)9ZM
27V)C2V
VXT)KMG
5W6)B2N
X2R)VJT
JQL)J91
YWP)89X
T42)V5M
2Z2)H63
GXJ)YWN
JJG)L7H
S3V)TL4
RFC)4LJ
YD5)QJ5
TNW)BWL
1B9)7NZ
P8G)6D1
YVD)BGV
GXX)RR7
C8W)CFB
YHM)J4D
FZX)SFP
D5S)FS2
FZC)YGK
PJL)Z8G
SF6)YG5
R42)414
57F)SF6
LRD)F9T
6YV)75H
1NW)ZK6
XN5)9HM
MN4)TRT
DKG)NF8
6PN)PS2
Y76)CNS
XWF)85R
D63)J9M
235)CYJ
414)R62
NQM)9JM
D9V)JP5
LVJ)WTF
D94)RYS
6H7)DHN
RBW)B1W
7SS)S2M
GT8)278
PWL)L9V
MXC)3K6
7HB)Q9M
9L5)DYR
TMS)4MC
9XQ)TD3
65K)Q6R
LTG)ZF1
WKK)RX3
ST3)6K3
YS6)7ZK
G9C)8YP
GHB)8Y6
PP4)QDD
QLD)7CF
QMG)KNS
WZ3)F31
PS3)KFB
FQC)YS2
HM1)T68
VYS)PDD
J8S)GR9
BWS)7M9
7RM)9QD
DWM)MFX
RQK)X2P
RGF)9FR
9KL)L48
7FG)PS3
2V1)7BJ
TFW)X4T
WHD)7ZF
YNY)72H
78D)9MZ
LG9)YBL
5BT)725
6B1)81D
MPD)MZN
KJM)YLS
XTS)GBK
MD5)RDS
2QV)8P3
45V)LTG
8KR)GYW
H9J)7V5
MJL)FFN
5SB)JD3
L7N)GDY
J1M)HSF
FVN)LTQ
NCN)T2H
R3X)C1Q
YFJ)S54
WXJ)HN7
38T)JNF
9RM)RBN
WCP)9M8
PZ9)BB4
RBM)9X7
CGM)8PQ
GQG)5G9
968)D3N
YKB)C59
JWT)MLP
RDD)XNY
KLR)S9V
COM)LGH
LY2)JC8
S6Z)LRG
WTX)7DF
HN7)BY5
8CC)NL4
655)TWM
6JV)NH2
GRG)SV3
5G8)NRZ
XG9)172
6GG)B7D
QCK)2RJ
NY3)QKQ
TRV)KVC
B92)83D
V21)Z1S
3LP)18M
3BX)LTN
3YV)VZZ
829)SS6
4SM)H6F
CCF)DZP
MBJ)SLW
XS2)V5H
HVR)4BC
R23)XZV
931)XGD
5DZ)W1N
3RB)VKY
JD3)F1W
B99)4F4
464)12L
9DV)2Z2
YTR)6J7
2VY)N86
4F4)NXW
6V5)YFW
KQP)BKJ
9H9)G91
KC6)4W2
7Q8)YOU
1M2)88F
9G4)JPD
9QD)YVF
T1S)DYD
NJ7)4SF
M94)3PJ
8GS)5JW
5G9)W7W
GPW)LN8
SDN)K7G
16X)CHH
1GQ)4KJ
SM5)6M4
D3N)31J
R9B)QBF
MVF)L7T
JPR)K1B
J6L)CC7
92N)BSM
TT5)RHW
NMF)18G
LTQ)LLF
9WS)9XQ
BMX)B5V
73P)8NF
SS8)JQT
NRF)JBB
L6D)SDN
6NX)QYW
VWR)G9C
4WL)KV4
YN9)D9V
X1G)6YW
XJV)NGK
ZSN)9H6
14S)9J3
Z9M)CZW
CYJ)FBT
347)WBN
JS9)6LX
DD2)293
738)7JR
V5V)5ZF
9FR)C5D
12L)VS5
84T)CR1
RN6)M5N
4W2)YWG
PXL)1B1
Q6R)7JF
PFK)Y79
TT2)7NM
WZ3)WD3
26C)X4J
PJL)69B
HJS)G95
W9T)H1W
TSN)6PN
PF1)NLV
7YL)VCS
52Q)G6P
3BZ)QS6
7KD)T6N
21X)MGN
5JV)DL7
ZPR)2C6
NYH)NPD
CGG)NG4
K8K)CKJ
1ZC)Y7C
NH2)HGW
YBW)5MR
7NM)T1J
X6H)DNH
TZ2)C13
F5G)VBR
WDX)K2S
486)858
76P)TNG
C4G)D5S
D4D)TSN
QX4)493
VQR)KKK
BJS)CM6
QX7)RCS
M15)7PT
KHD)F3B
4F2)49P
RP2)KSN
F9L)FZC
WXV)LRZ
M2Y)BHZ
XNY)DWM
D82)GMP
BHQ)GT8
5T1)XP9
3JX)BPM
38R)CS7
X4J)DTZ
X94)9WS
MGN)WKQ
9GV)TLC
8BX)TKM
ZRT)HXH
J4D)JK5
2B9)3RB
6B8)GHJ
16W)7XR
PD7)32Q
CNS)KQP
WZ7)5SB
CFB)K8H
LWS)2M1
2WF)G4B
BSP)J48
QYF)F9L
GD2)1TK
BFC)8FM
21N)4D8
4W5)7Z9
T68)C3N
GNY)X93
KSX)ZYP
ZYZ)LTW
37L)PP4
KDG)2B9
LRB)4WL
HNZ)BX5
2LR)MJL
GGB)THL
DLV)C3M
1V8)WDX
9MZ)FR7
NZ7)L5N
NRZ)GLM
TKM)7YB
8SW)7Q8
KV4)4FH
MC8)SVW
8SW)5KB
RMY)X6L
3PB)XBQ
RMW)5BT
BB4)VVL
F3S)TPB
BFG)KYX
3CV)F4K
B15)7PY
G4Y)JVN
P7L)Y3K
QTM)98D
D28)5TG
C1Z)J2L
VY2)V36
V1J)GW9
CKJ)WTX
LSN)TW9
CNX)ZHM
5Q7)1MV
MZN)4BX
H78)LSH
G7G)YXK
3HB)ZZN
ZRB)FW4
D69)6JD
PCN)MQH
N8C)TZ2
KNJ)HWR
MYQ)265
37S)GPY
7RB)B7Q
6YW)BKS
W2L)LXD
73H)K2H
G7G)1Q8
J2C)6GM
7ZF)ZRT
CWJ)7NY
3F6)CNH
VQB)D63
596)31R
SLW)FLV
Y3L)MYQ
XB2)FF3
8HQ)S2H
Z1S)NKR
8MX)HTX
N84)F6J
4WK)R1B
DPG)78D
K2S)8GS
VVB)63J
8CT)9F1
RNS)RY9
FGT)DN2
TWB)PYS
NVD)Z6T
7XM)R3X
R14)SBV
8Y4)Z5T
J48)PFC
Z5F)GRG
Z9R)X6H
SQX)8Q3
D5F)M7V
BX5)1G1
D82)RP2
N31)KWX
14J)BXB
SH6)41V
J6L)X9Q
XVS)S88
4KH)D85
72S)WMH
1K1)G4Y
MBZ)T4F
DYD)P8V
BBZ)GW2
FQC)S5H
TX1)WHD
R4T)N8L
KH9)6GZ
R1B)77D
JC8)3N6
C13)VV9
4PK)S33
KG5)6LV
9ZM)F5N
WMS)X6D
H8Y)BLL
NQH)W66
N8L)FZJ
YG5)5RN
TWM)LHK
DPD)DLV
Q9M)6LG
4PQ)PZD
DHN)SB4
X43)9L5
X9C)8W5
8KJ)GQG
CVY)F5Y
T5F)QPX
5F5)SQX
2XR)YBW
RY9)5R2
54K)CWR
KRD)BTB
YTY)NVP
6J7)KX5
3LL)L7N
1FP)LBK
V3R)K3Q
G2B)B62
K8H)MBJ
7TC)BJX
C8V)R14
1KW)L21
ZB2)JPR
P3W)C8V
GQ4)TT2
TWV)HBK
2XR)QJP
L8J)FNX
K14)KWL
F4J)KHT
49P)56S
GMT)K4S
FZ1)NSY
QYW)RMW
FVK)F3S
Q7Z)RGF
Q44)23V
C3K)TPY
5RN)D7B
QBN)11W
1S1)QVG
MFM)D7G
W4H)4CN
JVJ)4GB
1KQ)P1B
W4H)35Y
J4B)RFC
T95)RDF
Q9G)NYH
Y28)RTL
WWL)363
6DT)9YR
G95)G4J
R4S)YFL
C1Q)MC8
7DJ)72S
55B)5ZD
9R7)8VZ
9VH)JS9
41M)N43
KSN)6B8
RYS)1BW
5LY)88G
ZHD)CY2
4MC)P3H
YFL)PW3
69B)5DZ
S1X)LBB
B7D)PDF
9HM)S3V
WH2)M94
PX5)29T
K2N)TZT
VK9)VN8
JNF)3KH
DN2)H8F
8W4)FQ9
ZVB)B8W
FZX)5T1
X3Q)T42
3YT)21X
NGK)KFG
G91)8R2
8LS)C9P
6LG)N8C
JJZ)BJS
TNG)829
493)P7G
WLK)CWJ
4BX)G2B
81R)SNG
ZK6)SCN
L9D)7F1
S88)3Z1
53D)ZJV
6RY)PBD
8W5)2VL
JL1)MBZ
RJ4)RFX
LRG)QSB
QP3)HG3
HXH)DX8
4MC)R2C
NRZ)1LK
H9V)347
9N5)YRH
RWG)MKC
9G6)Z9R
3CS)FMK
5LF)JS8
5S9)MXQ
D7S)3WK
K3Q)HVR
DVC)DWN
3WS)WWC
F9T)2V1
GRP)4JN
3R9)2PD
4GB)WTB
H47)76P
WHM)R21
FR7)14H
5WP)64K
HLS)K14
LD6)KSX
NQM)9NM
NKR)DG3
WTB)ZRB
BVS)TCC
5JK)1ZC
GNV)ZHD
M7V)8CC
ZZN)9BG
C3S)2YC
RQW)TFW
QG1)QM6
D85)D3K
BCP)1TX
1XP)HF3
5LY)RN6
FZJ)37S
KF3)73P
MMV)55B
835)DBP
KFG)WKW
7BL)7VX
G6P)VP8
5H3)L8M
J91)3B5
1QX)3BX
QNC)PK6
MBC)BD3
Y9T)3F6
3N6)YS6
7SZ)3LP
XXH)655
C44)J3G
ZWF)41D
H7F)TK6
ZLD)5Q7
JWV)835
78K)ML8
LWR)1TL
GMJ)53D
ZNL)486
SBH)DY6
3LR)8LF
7Y3)7KK
6M4)JMM
PD9)GV9
CY2)XQP
ZYP)ZP9
5GV)CKM
M6M)FK8
SCN)GRN
PFK)JT8
C13)B92
QDX)JJZ
HJ8)QRZ
W7J)J4G
6YY)TRH
XCD)L8J
5MP)5N5
TMQ)9DV
SNV)65K
SWZ)TYL
HWR)K2N
PZD)CJ3
CYY)PXW
414)TWR
63J)X9C
W85)S8Z
VV9)LRD
VQY)MW3
H6M)QCK
PTQ)R29
688)YFJ
CC8)KF3
YRG)KZG
PBQ)XB2
9M8)WYD
Y3K)4JW
9FH)H9J
QPX)VK9
68G)9HZ
FVC)JWR
H1M)8KJ
BTB)FPR
6LX)ZLT
83L)7LJ
ZP9)CLH
246)MZ9
ZQK)246
LTW)LVK
88F)JNT
BXB)2VM
2TW)PZM
BQX)CGG
WKW)WDF
PPH)GYD
RTL)B99
J9M)M6P
LGH)BNM
MBQ)16W
G7Q)1XP
LBB)XVS
FS2)P68
L9V)85V
3PJ)RWJ
HX4)8HQ
FGD)3CR
D66)GXX
41V)T3N
RY5)2C9
DX6)J5T
TWR)8KR
KNV)VTM
2D4)92N
MZH)4H8
KTQ)VY2
6JD)BBZ
741)7BD
C2V)VQB
7Z9)NQH
LL9)B14
32Q)BBM
W2L)YZG
6ZK)RNQ
WWM)376
XQP)DXX
1LK)DW8
FQ9)6XK
YPX)5ZH
ZWX)8SW
1SJ)9L7
QY1)CGM
9NM)V8R
1K5)2VT
GRN)256
CFK)HJ8
V36)3BM
18M)LWS
1FP)PFK
2Y4)BD2
WQ6)QSK
VTM)FCP
GW9)ZWX
QNY)SJG
T3N)5W6
L56)QFS
L7T)WKK
JPD)7RM
4GB)153
7VX)RQQ
FKT)ZZ3
YGK)HZY
Z65)PBQ
MJC)CGN
PCN)5GV
V2V)2D4
R29)V97
C4L)DC5
26N)8JS
1VY)4KH
56S)FGD
QDK)G71
5KB)W2L
XN5)J2D
THL)HHN
YJ6)NCN
BT7)YVH
DWF)V1J
P1B)FQC
256)1XN
CS7)D53
NF8)B23
JPV)8J4
8Z8)GLR
LG9)9G4
N99)44R
3BM)8BX
QL6)16H
CR1)KRC
VW5)1K1
L7H)2W1
GYH)ZSN
XNT)ZM7
THL)L5C
KPP)5FP
CM6)J9S
RBM)VXT
5H6)JFH
8YP)ZB2
2M1)FVK
K9N)YYN
G4J)LY2
7LJ)DRS
JFH)7FG
R42)89T
KQR)182
BGV)CCF
7HS)3P9
CWR)BBG
C3N)GNY
2V3)3R9
331)PWL
GYW)5N2
CW1)T56
7MF)F4J
97J)14J
LSH)426
W1N)8Z8
K14)ZNJ
ZJV)DJY
7KK)V6D
M5N)2Y4
TYL)8TY
Y4K)78K
S2H)PP3
LNH)YYR
PDF)97J
NSY)2V3
WKT)F7Q
V4Z)PD7
ZPM)K5Z
QJ5)7MF
C3M)ZPR
4DH)MBC
426)JXW
F5N)PJL
6GZ)6GS
ZRH)G1B
KTQ)3PB
KMG)F56
JQT)C3K
HKV)SWQ
4JW)XR4
5NY)P8G
7H5)7DW
1Q8)464
CDP)SBH
FMH)8MX
H1W)WJZ
DC5)HJS
J2D)3LR
K4S)L56
SVW)XR9
RFX)G59
KRC)ZLD
JRP)6MH
5FP)KQD
JF8)M6M
DGY)5H3
5R2)1B9
3N6)VJG
2TY)N3Q
TD3)J3X
J4G)5MP
52D)6NX
R4P)L52
Z8G)4F2
L5C)37W
NVP)FZ1
1RX)3ZH
LPL)6DT
278)9Z7
M3W)W4H
84Z)Z51
RSS)KC6
7JF)11D
ZV3)QYF
TCC)L9D
1V6)C4G
XZV)LQX
8TY)N8B
13W)R23
Y6S)8KQ
LXD)1S1
85V)PNB
SNT)JPV
153)FZX
1G1)NR2
KVC)VY8
641)8RG
1HG)CDY
BK1)Q9G
W67)WZ5
ZPH)GYH
W9Y)GRP
BKS)V4Z
NZ7)SNV
NTK)XCY
9D1)26C
HZY)5F5
WYD)GT3
MZ9)JJX
QDD)P3W
X4T)13K
P2J)N84
YWG)22P
VJW)39N
14H)LPL
293)VQY
FJY)C1P
JMM)K4H
6GM)27M
BBM)LGD
ZF1)V7T
G41)XR2
JVN)FYP
WQ2)YQV
LN8)BQY
17R)TR1
4H8)W85
1WK)B8P
QPD)7Y3
6J8)XCD
21L)NZV
PC7)KQR
DL7)HK6
B62)NQM
CGM)D94
RLH)TT5
ZDK)J6L
32P)96X
T1J)5H6
B2D)4JB
VV7)6WC
QTM)VWR
ZXS)9GV
7ZQ)8L4
Z51)685
G71)H6M
JBB)37L
3CR)B58
HNC)T1P
89T)ZJ8
RBC)CFK
7V5)SWZ
5XY)54K
YYN)VTX
KYX)7KD
D94)LL9
FVH)KHD
B78)89G
8FF)NLQ
TQB)6V7
Z3Q)MD4
WL7)3GK
6K3)NWK
TLK)PCN
3M1)GMJ
Y79)C1G
RBN)52Q
JS8)TCF
VVL)XS2
H9M)FMB
Z6Y)768
J3G)3M1
M6P)XBP
2VL)KRD
439)R4S
5ZF)QDX
TK6)TNW
PY2)439
1XN)BXF
WS3)YWP
11W)SCQ
9HZ)JZF
8T4)WDD
SFP)XBT
PHL)15K
YSL)R36
F1W)CVY
YS6)M15
1SJ)RQK
CJ3)6GG
1MV)PXL
8W5)KBF
6GS)641
L6M)4PQ
WWZ)WWM
M4Y)3CY
CKM)NMG
C1G)Y6S
ZLT)QNC
WSB)TWV
H8F)D5F
DG3)NVD
RC7)6CZ
9Z7)X13
5TG)V25
KFB)27V
4HV)6JV
P68)TJZ
2SX)KCP
QM6)FR2
2Y1)H6S
RDF)GGB
XBQ)N62
ZMW)RNS
DTZ)WHM
F4K)VB9
W66)1HG
C1G)FJV
CWQ)7R5
61N)WS3
CLH)H9V
P3W)DD2
7GG)ZBZ
PXW)DQ8
GDY)2WF
KNS)X3Q
6WC)BFG
9MT)XH8
KBF)6H7
V97)Z4D
QJP)3BF
KWX)DPD
FS6)7SP
N3G)4WK
NSY)VV7
4V5)F79
11D)2SX
6NX)FMH
MQH)VYS
SV3)RY5
FJV)GLS
WDD)NX5
FT7)NRF
GT3)2VY
PW3)5LF
H6S)X43
18X)XC3
MJR)Y8X
TKM)KFK
Y9D)WZ7
9B6)GQ4
4HV)X2R
NWK)5WP
XGD)MXK
JT8)MJX
P8V)W1Y
6B1)TQB
J5T)PJD
RQQ)QCG
PNH)FJY
ZKC)8T4
TW9)17R
8L3)M4Y
K2H)FKT
N7N)YYX
3H4)X94
4CN)2TW
XKQ)WFS
R23)T9K
YVF)ZKC
BD2)QBN
QSK)5VG
DW8)V3R
N3Q)PPH
FYP)GMT
1P7)L6D
8J4)F5G
8Y6)RQW
N8V)M2J
ZM7)HDW
XG9)3YT
9G6)T53
P7G)38R
C9P)9D1
XM5)WBJ
FVT)ST3
NX5)YKP
8R2)4SM
LCL)8CT
WCP)5JM
221)J18
MLP)N6H
18X)K9N
393)GDS
182)Z3Q
WMY)WGJ
9JM)QQ6
FZZ)RDD
6YW)CDP
9X7)VGQ
PK6)41M
8P3)YD5
9SV)W3L
M4N)W67
F7Q)ZRH
VCS)P2J
QSB)T95
NXW)MG7
SS6)HNS
V8G)R4P
2DB)BVL
7DF)WMS
6QS)RGC
9D1)865
54X)JF8
81D)FZZ
172)Y9D
8VZ)WSB
GDS)M14
MG7)P8F
7RB)H1M
TPY)XXH
9L7)VVB
BD3)ZPM
369)13W
TRH)MD7
MFX)QMG
8KQ)FZR
LXP)XN5
BVP)H78
P5T)SP4
Z6T)4V5
KYX)PDV
NZV)1MW
9YR)ZKH
M2J)QY1
6V7)5T2
W9H)SAN
BZX)7RL
QQ6)52D
HBK)9KL
78K)XJV
NKZ)X7T
Z51)C44
JWR)BHQ
JLW)CVV
9RM)N8V
WRL)6V5
BBL)DGZ
PJD)DJP
RX3)KDG
XR9)D69
2Y4)YW9
TJN)7RC
CHH)SQL
72H)WCP
M14)16J
2N2)XM5
TWY)XZS
JD7)R9H
6XK)738
YRH)18X
CZW)LCL
K1B)ZV3
BQY)W1L
61H)ZVB
YYR)XG9
WBN)4W5
N62)BBL
5LL)KG5
7LX)DR3
KQD)C7V
5ZD)CW1
2L5)MPD
F8R)X6S
QVG)21L
KHD)SNC
7SP)N9L
J9S)7HS
8NF)2TY
GHJ)9SV
C7V)3HB
JKG)1P7
7KP)V21
YBL)2QV
HTX)TJJ
V7T)LXP
DX6)JWZ
RNQ)TD4
333)Z65
768)16X
7L5)WX7
6VK)1SJ
39N)WRL
R5R)RBW
YW1)JJG
SP2)2N2
GV9)WL6
P59)JGV
PDF)G89
NDD)WVY
2JP)MZW
QJP)7SS
WL6)VJW
B2N)B55
BBZ)H8T
PYS)DGY
BPM)HS8
5MR)RYC
FLV)K8K
SNG)YSL
ZJ8)8L3
WGJ)DX6
MNV)H27
7JR)NZ7
GLS)YJ6
3RC)NJ7
FJV)R42
B7D)SM5
WQZ)D4D
KKK)7L5
78G)K9D
XP9)1FP
ZV6)F4X
FGL)2Y1
VP8)M93
5SB)38X
RN3)N66
CDY)LWR
WMH)L6M
KX5)5G8
JWZ)W7J
DR3)SH6
5N5)5NY
ZHM)MBQ
K4H)YTY
4LJ)14S
PMB)2HK
MRP)C8W
27M)CC8
PDD)JYW
1FF)GR6
KXH)BZ1
7RC)1GQ
FMB)PNH
MJX)1V6
BBG)QK6
V25)J4B
PP3)PC7
D7B)WSV
3B5)PR1
7BD)XVB
N9L)8L8
61H)DQS
K9D)1V8
LQN)393
89X)BK1
B14)ZYZ
4KJ)NRK
ZCV)FVC
X6D)WQ2
13K)7LX
7Z9)RWG
1YB)BH3
6MH)3ZT
XR2)6B1
3P9)H4S
SBV)SHH
XM2)1K5
VB9)NMF
2RJ)HHB
89G)QDB
TD4)9R7
L21)BT7
9G9)LNH
G59)FFQ
SWQ)XWF
FBT)688
FFQ)JVJ
D7G)GHB
K9T)7YL
MKC)235
5JM)WD5
SMF)TLK
F31)9MT
8LF)BQX
5N2)4QW
2VM)R4T
FMK)HH2
W1N)Q44
JY1)7HB
8L4)B2D
CVL)7H5
SHH)GKP
TSG)FT7
WBJ)G1Z
HS8)5JV
BY5)RBM
LNP)PZ9
3BF)WD7
RDS)5ST
JYW)LD6
XBP)XNT
7HH)84T
93Q)1S2
ZV3)9RM
J3X)Y36
LD6)S1X
JWZ)NDD
TZT)8LS
W7W)P5T
K5Z)ZXS
WZ5)7GG
ZMG)QL4
HDW)JC1
GK5)NTK
VCS)V9L
VZZ)QPD
FL6)BZX
SNC)HNC
S6Z)Q57
FZC)BVS
S5H)GNV
T8B)5MQ
FF3)BWS
SCQ)PF1
41D)3Y2
Z4D)Y4K
FCP)DPG
N9L)J8S
H63)GXJ
T9K)S6Z
BWL)DKG
PZM)JRP
SP4)WWL
ZKH)PMB
DVC)5LL
LVK)QL6
PBD)2C2
Y98)8W4
1SZ)ZV6
SNC)331
R2C)W9H
KWL)NY3
893)221
DZD)32P
T1P)WKT
C1P)BCP
5KJ)R9B
YWN)369
FK8)9VH
31R)R5R
38X)Z6Y
S8Z)PY2
S33)RN3
16J)D28
JRP)T8B
77D)1XW
K4H)3BZ
DWN)GX8
QBF)WXV
31J)7BL
Y2W)D82
WTF)Q78
VTX)5LY
75H)WWZ
9VH)YW1
S54)XKQ
BZ1)7DJ
DGZ)7XM
MXQ)B15
HH2)1KQ
N86)QX7
FPR)ZDK
2C2)HX4
YYX)Z3S
GMP)XM2
F56)C3S
3YG)ZGD
7YS)HNZ
B2D)6YV
R36)S8M")

(defn read-orbits
  ([]
   (read-orbits day-5-orbits))
  ([orbits]
   (with-in-str orbits
     (loop [orbit (read-line)
            map {}]
       (if orbit
         (recur (read-line)
                (let [[anchor orbiter] (clojure.string/split orbit #"\)")]
                  (update map anchor (fnil conj #{}) orbiter)))
         map)))))

(defn count-orbits
  ([]
   (count-orbits (read-orbits) 0 "COM"))
  ([system base planet]
   (loop [orbiters (get system planet)
          current base]
     (println current planet base orbiters)
     (if (seq orbiters)
       (apply + current (map (partial count-orbits system (inc current)) orbiters))
       current))))
