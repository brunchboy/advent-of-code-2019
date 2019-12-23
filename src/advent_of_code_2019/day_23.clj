(ns advent-of-code-2019.day-23
  "Solutions to the Day 23 problems."
  (:require [clojure.core.async :as a :refer [>! <! >!! <!!]]
            [advent-of-code-2019.day-9 :refer [assoc-growing intcode]]
            [clojure.repl :refer :all]))

(def program
  "The network driver intcode program."
  [3,62,1001,62,11,10,109,2241,105,1,0,2066,1569,571,1239,2200,1342,2169,899,1173,2035,1540,1204,1637,1806,641,977,1272,1773,1144,1008,1699,2107,1107,1909,1872,833,802,1410,932,1072,1307,1938,2000,734,1837,2138,1039,1606,701,1373,608,866,1443,1478,771,1509,672,1969,1668,1738,0,0,0,0,0,0,0,0,0,0,0,0,3,64,1008,64,-1,62,1006,62,88,1006,61,170,1105,1,73,3,65,20102,1,64,1,21002,66,1,2,21102,105,1,0,1106,0,436,1201,1,-1,64,1007,64,0,62,1005,62,73,7,64,67,62,1006,62,73,1002,64,2,132,1,132,68,132,1002,0,1,62,1001,132,1,140,8,0,65,63,2,63,62,62,1005,62,73,1002,64,2,161,1,161,68,161,1102,1,1,0,1001,161,1,169,101,0,65,0,1102,1,1,61,1102,1,0,63,7,63,67,62,1006,62,203,1002,63,2,194,1,68,194,194,1006,0,73,1001,63,1,63,1106,0,178,21102,210,1,0,105,1,69,1202,1,1,70,1101,0,0,63,7,63,71,62,1006,62,250,1002,63,2,234,1,72,234,234,4,0,101,1,234,240,4,0,4,70,1001,63,1,63,1106,0,218,1105,1,73,109,4,21102,1,0,-3,21102,0,1,-2,20207,-2,67,-1,1206,-1,293,1202,-2,2,283,101,1,283,283,1,68,283,283,22001,0,-3,-3,21201,-2,1,-2,1106,0,263,21202,-3,1,-3,109,-4,2105,1,0,109,4,21101,1,0,-3,21101,0,0,-2,20207,-2,67,-1,1206,-1,342,1202,-2,2,332,101,1,332,332,1,68,332,332,22002,0,-3,-3,21201,-2,1,-2,1105,1,312,21202,-3,1,-3,109,-4,2105,1,0,109,1,101,1,68,359,20101,0,0,1,101,3,68,367,20102,1,0,2,21101,0,376,0,1105,1,436,22101,0,1,0,109,-1,2106,0,0,1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192,16384,32768,65536,131072,262144,524288,1048576,2097152,4194304,8388608,16777216,33554432,67108864,134217728,268435456,536870912,1073741824,2147483648,4294967296,8589934592,17179869184,34359738368,68719476736,137438953472,274877906944,549755813888,1099511627776,2199023255552,4398046511104,8796093022208,17592186044416,35184372088832,70368744177664,140737488355328,281474976710656,562949953421312,1125899906842624,109,8,21202,-6,10,-5,22207,-7,-5,-5,1205,-5,521,21101,0,0,-4,21101,0,0,-3,21101,0,51,-2,21201,-2,-1,-2,1201,-2,385,471,20102,1,0,-1,21202,-3,2,-3,22207,-7,-1,-5,1205,-5,496,21201,-3,1,-3,22102,-1,-1,-5,22201,-7,-5,-7,22207,-3,-6,-5,1205,-5,515,22102,-1,-6,-5,22201,-3,-5,-3,22201,-1,-4,-4,1205,-2,461,1106,0,547,21102,1,-1,-4,21202,-6,-1,-6,21207,-7,0,-5,1205,-5,547,22201,-7,-6,-7,21201,-4,1,-4,1105,1,529,21201,-4,0,-7,109,-8,2106,0,0,109,1,101,1,68,563,21002,0,1,0,109,-1,2106,0,0,1102,6581,1,66,1101,0,4,67,1101,598,0,68,1101,0,253,69,1101,0,1,71,1101,606,0,72,1106,0,73,0,0,0,0,0,0,0,0,27,85159,1102,6151,1,66,1101,0,2,67,1102,635,1,68,1101,302,0,69,1102,1,1,71,1101,0,639,72,1105,1,73,0,0,0,0,1,292724,1102,84653,1,66,1101,0,1,67,1101,0,668,68,1101,0,556,69,1101,0,1,71,1102,1,670,72,1106,0,73,1,11,34,14669,1102,96893,1,66,1101,0,1,67,1102,699,1,68,1102,556,1,69,1102,0,1,71,1102,701,1,72,1106,0,73,1,1705,1101,93557,0,66,1101,2,0,67,1101,728,0,68,1102,302,1,69,1101,1,0,71,1102,1,732,72,1106,0,73,0,0,0,0,41,155138,1101,102061,0,66,1101,4,0,67,1102,761,1,68,1102,1,302,69,1102,1,1,71,1102,1,769,72,1105,1,73,0,0,0,0,0,0,0,0,4,122394,1101,0,93497,66,1102,1,1,67,1102,1,798,68,1101,556,0,69,1101,1,0,71,1102,800,1,72,1106,0,73,1,-18,49,41897,1101,0,80071,66,1102,1,1,67,1101,0,829,68,1101,0,556,69,1102,1,1,71,1102,1,831,72,1105,1,73,1,-1474987,2,13162,1101,71473,0,66,1101,2,0,67,1102,860,1,68,1101,0,351,69,1102,1,1,71,1102,1,864,72,1105,1,73,0,0,0,0,255,32771,1101,77569,0,66,1101,0,2,67,1102,1,893,68,1102,1,302,69,1101,1,0,71,1102,1,897,72,1106,0,73,0,0,0,0,39,52396,1102,24799,1,66,1102,1,1,67,1101,926,0,68,1101,0,556,69,1101,2,0,71,1102,1,928,72,1105,1,73,1,211,39,13099,11,42299,1102,55399,1,66,1102,1,1,67,1101,959,0,68,1102,1,556,69,1101,8,0,71,1102,961,1,72,1105,1,73,1,2,41,77569,39,26198,27,170318,17,79714,3,78889,34,44007,4,61197,4,81596,1101,0,22993,66,1101,1,0,67,1102,1,1004,68,1102,1,556,69,1102,1,1,71,1101,0,1006,72,1105,1,73,1,2485433,2,19743,1101,0,32687,66,1101,0,1,67,1101,0,1035,68,1102,556,1,69,1102,1,1,71,1102,1037,1,72,1106,0,73,1,28661,16,15501,1102,1,44657,66,1101,1,0,67,1102,1,1066,68,1102,556,1,69,1102,1,2,71,1102,1,1068,72,1106,0,73,1,10,33,102061,4,40798,1102,53777,1,66,1101,3,0,67,1101,1099,0,68,1101,0,302,69,1101,0,1,71,1101,0,1105,72,1106,0,73,0,0,0,0,0,0,22,212991,1101,70997,0,66,1101,4,0,67,1102,1,1134,68,1102,1,253,69,1101,0,1,71,1101,0,1142,72,1106,0,73,0,0,0,0,0,0,0,0,38,93557,1102,93761,1,66,1101,1,0,67,1102,1171,1,68,1102,556,1,69,1102,1,0,71,1102,1,1173,72,1106,0,73,1,1625,1101,0,96973,66,1102,1,1,67,1102,1,1200,68,1102,556,1,69,1102,1,1,71,1102,1202,1,72,1105,1,73,1,13,24,102071,1101,0,42299,66,1102,1,3,67,1101,0,1231,68,1101,302,0,69,1102,1,1,71,1101,0,1237,72,1105,1,73,0,0,0,0,0,0,42,82763,1102,78889,1,66,1102,2,1,67,1102,1,1266,68,1102,1,302,69,1102,1,1,71,1102,1270,1,72,1105,1,73,0,0,0,0,34,29338,1102,5167,1,66,1102,3,1,67,1101,0,1299,68,1101,0,302,69,1101,0,1,71,1102,1,1305,72,1106,0,73,0,0,0,0,0,0,22,70997,1101,12697,0,66,1101,0,1,67,1101,1334,0,68,1101,0,556,69,1101,3,0,71,1101,0,1336,72,1105,1,73,1,5,33,306183,33,408244,4,20399,1102,1,99251,66,1101,0,1,67,1102,1369,1,68,1102,556,1,69,1102,1,1,71,1101,0,1371,72,1106,0,73,1,181,24,204142,1102,1,13099,66,1102,1,4,67,1102,1400,1,68,1101,302,0,69,1102,1,1,71,1102,1408,1,72,1106,0,73,0,0,0,0,0,0,0,0,1,73181,1102,1,85159,66,1102,1,2,67,1102,1437,1,68,1102,1,302,69,1101,1,0,71,1101,0,1441,72,1106,0,73,0,0,0,0,17,39857,1102,1,82763,66,1102,1,3,67,1101,1470,0,68,1102,1,302,69,1102,1,1,71,1101,0,1476,72,1106,0,73,0,0,0,0,0,0,1,146362,1102,44179,1,66,1102,1,1,67,1101,1505,0,68,1102,556,1,69,1102,1,1,71,1102,1507,1,72,1105,1,73,1,278,29,161331,1102,1,19457,66,1102,1,1,67,1101,1536,0,68,1102,1,556,69,1102,1,1,71,1102,1,1538,72,1105,1,73,1,17,39,39297,1101,0,60251,66,1101,0,1,67,1102,1567,1,68,1102,1,556,69,1102,1,0,71,1101,1569,0,72,1106,0,73,1,1621,1101,73181,0,66,1102,4,1,67,1101,0,1596,68,1101,0,253,69,1102,1,1,71,1102,1,1604,72,1105,1,73,0,0,0,0,0,0,0,0,25,71473,1101,0,99277,66,1102,1,1,67,1102,1633,1,68,1102,556,1,69,1101,1,0,71,1102,1635,1,72,1106,0,73,1,-661,24,408284,1102,1,2957,66,1101,1,0,67,1101,1664,0,68,1101,556,0,69,1101,1,0,71,1101,1666,0,72,1106,0,73,1,16,38,187114,1102,77687,1,66,1101,0,1,67,1101,1695,0,68,1102,1,556,69,1101,1,0,71,1101,1697,0,72,1105,1,73,1,160,4,101995,1102,79757,1,66,1102,1,1,67,1102,1,1726,68,1101,0,556,69,1101,5,0,71,1102,1728,1,72,1105,1,73,1,1,49,125691,29,53777,24,306213,16,5167,11,84598,1102,41897,1,66,1102,3,1,67,1102,1,1765,68,1102,302,1,69,1102,1,1,71,1102,1,1771,72,1105,1,73,0,0,0,0,0,0,22,141994,1102,1,39857,66,1102,2,1,67,1102,1800,1,68,1102,302,1,69,1102,1,1,71,1101,0,1804,72,1105,1,73,0,0,0,0,3,157778,1101,87671,0,66,1101,1,0,67,1102,1833,1,68,1102,556,1,69,1101,1,0,71,1101,1835,0,72,1105,1,73,1,64937,49,83794,1102,14669,1,66,1102,1,3,67,1102,1864,1,68,1101,0,302,69,1101,1,0,71,1101,0,1870,72,1105,1,73,0,0,0,0,0,0,40,6151,1101,0,102071,66,1102,4,1,67,1101,1899,0,68,1101,302,0,69,1102,1,1,71,1101,1907,0,72,1106,0,73,0,0,0,0,0,0,0,0,22,283988,1101,93169,0,66,1101,1,0,67,1102,1936,1,68,1101,0,556,69,1101,0,0,71,1101,1938,0,72,1105,1,73,1,1087,1102,1,92173,66,1101,0,1,67,1102,1,1965,68,1102,1,556,69,1101,1,0,71,1101,0,1967,72,1105,1,73,1,7751410,2,26324,1101,80897,0,66,1101,1,0,67,1102,1996,1,68,1101,0,556,69,1101,1,0,71,1102,1,1998,72,1106,0,73,1,-1275519,2,6581,1101,0,102481,66,1101,3,0,67,1101,2027,0,68,1102,302,1,69,1101,0,1,71,1101,0,2033,72,1106,0,73,0,0,0,0,0,0,1,219543,1102,1,2687,66,1102,1,1,67,1101,2062,0,68,1102,1,556,69,1101,1,0,71,1102,1,2064,72,1106,0,73,1,5807,29,107554,1101,0,32771,66,1101,0,1,67,1102,2093,1,68,1101,0,556,69,1102,6,1,71,1101,0,2095,72,1105,1,73,1,22083,40,12302,42,165526,42,248289,32,102481,32,204962,32,307443,1102,2791,1,66,1101,0,1,67,1101,0,2134,68,1101,0,556,69,1101,1,0,71,1102,2136,1,72,1106,0,73,1,-204,11,126897,1101,5669,0,66,1102,1,1,67,1102,2165,1,68,1101,556,0,69,1101,1,0,71,1101,2167,0,72,1106,0,73,1,125,33,204122,1101,94693,0,66,1102,1,1,67,1101,2196,0,68,1101,556,0,69,1101,1,0,71,1101,2198,0,72,1105,1,73,1,244,16,10334,1101,0,20399,66,1101,0,6,67,1102,1,2227,68,1101,0,302,69,1101,1,0,71,1102,1,2239,72,1106,0,73,0,0,0,0,0,0,0,0,0,0,0,0,25,142946])

(def idle-addresses
  "Holds the addresses of all computers that have reported themselves idle."
  (atom #{}))

(defn intcode-async-non-blocking
  "Alternate version of the day 9 asynchronous intcode interpreter,
  where the input opcode returns `-1` when there is no input available
  instead of blocking. Also has some hackery to support part 2."
  ([input-chan output-chan]
   (intcode-async-non-blocking program input-chan output-chan))
  ([program-data input-chan output-chan]
   (future
     (let [our-address (atom nil)]
       (loop [pc       0 ; Program counter: address of next instruction to be executed.
              rel-base 0 ; Relative addressing mode base address.
              memory   program-data]  ; The entire memory of the computer, initialized with the program contents.
         (let [load-from-pc    (fn [offset] (nth memory (+ pc offset) 0))
               instruction     (load-from-pc 0)
               opcode          (rem instruction 100)
               modes           (clojure.string/reverse (str (quot instruction 100)))
               resolve-operand (fn [index]  ; Handles mode for a value being loaded, which can be immediate.
                                 (let [value (load-from-pc (inc index))
                                       mode  (nth modes index \0)]
                                   (case mode
                                     \0 (nth memory value 0)
                                     \1 value
                                     \2 (nth memory (+ rel-base value) 0))))
               resolve-address (fn [index]  ; Handles mode for a value being stored, which can never be immediate.
                                 (let [value (load-from-pc (inc index))
                                       mode  (nth modes index \0)]
                                   (case mode
                                     \0 value
                                     \2 (+ rel-base value))))]
           #_(println "pc:" pc "opcode:" opcode "modes:" modes "rel-base:" rel-base #_"mem:" #_memory)

           ;; Decode an instruction and compute appropriate new values for
           ;; the program counter, relative addressing mode base, memory,
           ;; and perform I/O on the input/output channels when needed.
           (case opcode

             ;; Add operand 0 to operand 1, storing the result in address specified by operand 2.
             1 (recur (+ pc 4) rel-base
                      (assoc-growing memory (resolve-address 2) (+ (resolve-operand 0) (resolve-operand 1))))

             ;; Multiply operand 0 by operand 1, storing the result in address specified by operand 2.
             2 (recur (+ pc 4) rel-base
                      (assoc-growing memory (resolve-address 2) (* (resolve-operand 0) (resolve-operand 1))))

             ;; Read an input value, storing it in the address specified by operand 0. Stores -1 if no input was ready.
             ;; Also keeps track of the address we were assigned as our first input, and uses it to report when we are
             ;; (or are not) idle.
             3 (let [timeout-chan (a/timeout 10)
                     [input-result ch] (a/alts!! [timeout-chan input-chan])]
                 (try
                   (if (= ch timeout-chan)
                     (when-let [addr @our-address] (swap! idle-addresses conj addr))
                     (if-let [addr @our-address]
                       (swap! idle-addresses disj addr)
                       (reset! our-address input-result)))
                   (catch Throwable t
                     (println "Problem performing input instruction:" t)))
                 (recur (+ pc 2) rel-base
                        (assoc-growing memory (resolve-address 0) (or input-result -1))))


             ;; Output the value specified by operand 0.
             4 (recur (+ pc 2) rel-base
                      (do
                        (>!! output-chan (resolve-operand 0))
                        memory))

             ;; If the value of operand 0 is true (non-zero) jump to the address found in operand 1.
             5 (recur (if (zero? (resolve-operand 0))
                        (+ pc 3)
                        (resolve-operand 1))
                      rel-base
                      memory)

             ;; If the value of operand 0 is false (zero) jump to the address found in operand 1.
             6 (recur (if (zero? (resolve-operand 0))
                        (resolve-operand 1)
                        (+ pc 3))
                      rel-base
                      memory)

             ;; Store 1 in the address specified by operand 2 if operand 0 is less than operand 1, otherwise store 0.
             7 (recur (+ pc 4) rel-base
                      (assoc-growing memory (resolve-address 2)
                                     (if (< (resolve-operand 0) (resolve-operand 1)) 1 0)))

             ;; Store a 1 in the address specified by operand 2 if operand 0 equals operand 1, otherwise store a 0.
             8 (recur (+ pc 4) rel-base
                      (assoc-growing  memory (resolve-address 2)
                                      (if (= (resolve-operand 0) (resolve-operand 1)) 1 0)))

             ;; Add the value of operand zero to the relative address base.
             9 (recur (+ pc 2) (+ rel-base (resolve-operand 0)) memory)

             ;; End the program, closing the output channel.
             99 (a/close! output-chan))))))))

(def network-computer-count
  "The number of network computers to create."
  50)

(def channel-size
  "The number of messages to buffer in the channels used to
  communicate with the network computers."
  1000)

(defn accumulate-packet
  "Adds another segment to the packet being built for the supplied
  channel. If it has reached its full three-value. Returns the updated
  packet buffers map. If the packet has reached its full size (of
  three values), it is returned as the second argument, and the
  returned packet buffer for that channel is returned empty."
  [packet-buffers val chan]
  (let [packet (conj (get packet-buffers chan []) val)]
    (if (> (count packet) 2)
      [(dissoc packet-buffers chan) packet] ; Finished building this packet, return it.
      [(assoc packet-buffers chan packet)]))) ; Still building, just accumulate the new value segment.

(defn part-1
  "Solve part 1."
  []
  ;; Set up the channels used to communicate with the network
  ;; computers, priming the input channels with the address assignment
  ;; of each.
  (let [input-channels  (mapv (fn [address]
                                (let [result (a/chan channel-size)]
                                  (>!! result address)
                                  result))
                              (range network-computer-count))
        output-channels (vec (for [_ (range network-computer-count)]
                               (a/chan channel-size)))]

    (println "Created" (count input-channels) "input and" (count output-channels) "output channels.")

    ;; Start all the network computers, connecting them with their I/O channels.
    (doseq [[input output] (partition 2 (interleave input-channels output-channels))]
      (intcode-async-non-blocking input output))

    (println "Starting driver loop.")
    (loop [packet-buffers {} ; Builds up packets indexed by output channel.
           [val chan]     (a/alts!! output-channels)] ; Get the first value available from a computer.
      #_(println "Received" val)
      (let [[packet-buffers finished-packet] (accumulate-packet packet-buffers val chan)]
        (when finished-packet (println "Received packet" finished-packet))
        (if-let [[address x y] finished-packet] ; We have a packet to process.
          (if (= address 255)
            y ; We have the answer to the problem! Just return it.
            (let [destination (nth input-channels address :invalid)] ; We have a packet to send to another computer.
              (if (= :invalid address)
                (println "Ignoring packet sent to invalid address:" finished-packet)
                (do ; Send the packet to the specified input channel.
                  (>!! destination x)
                  (>!! destination y)))
              (recur packet-buffers ; And wait for the next available value.
                     (a/alts!! output-channels))))
          (recur packet-buffers (a/alts!! output-channels)))))))

(def last-nat-packet
  "Holds the last packet received by the NAT."
  (atom nil))

(defn part-2
  "Solve part 2."
  []
  (let [input-channels  (mapv (fn [address]
                                (let [result (a/chan channel-size)]
                                  (>!! result address)
                                  result))
                              (range network-computer-count))
        output-channels (vec (for [_ (range network-computer-count)]
                               (a/chan channel-size)))]

    (println "Created" (count input-channels) "input and" (count output-channels) "output channels.")

    ;; Start all the network computers, connecting them with their I/O channels.
    (doseq [[input output] (partition 2 (interleave input-channels output-channels))]
      (intcode-async-non-blocking input output))

    (println "Starting NAT idle daemon.");
    (future
      (loop [last-y nil]
        (if (= (count @idle-addresses) 50) ; The computers are all idle.
          (let [destination (first input-channels)] ; Send the last packet the NAT received to address 0.
            (if-let [[_ x y] @last-nat-packet]
              (if (= y last-y)
                (println "Found solution:" y) ; We have our answer! Return it and shut down.
                (do
                  (reset! last-nat-packet nil) ; Don't send same packet more than once.
                  (>!! destination x)
                  (>!! destination y)
                  (println "NAT detected idle network, sent" x y "to address 0.")
                  (recur y))) ; Note what Y value we sent in case we send it immediately again.
              (recur last-y)))
          (recur last-y))))

    (println "Starting driver loop.")
    (loop [packet-buffers {} ; Builds up packets indexed by output channel.
           [val chan]     (a/alts!! output-channels)] ; Get the first value available from a computer.
      #_(println "Received" val)
      (let [[packet-buffers finished-packet] (accumulate-packet packet-buffers val chan)]
        (when finished-packet (println "Received packet" finished-packet))
        (if-let [[address x y] finished-packet] ; We have a packet to process.
          (if (= address 255) ; Something for the NAT
            (do
              (reset! last-nat-packet finished-packet) ; Remember this to send when the network goes idle.
              (recur packet-buffers (a/alts!! output-channels))) ; Keep looking for input.
            (let [destination (nth input-channels address :invalid)] ; We have a packet to send to another computer.
              (if (= :invalid address)
                (println "Ignoring packet sent to invalid address:" finished-packet)
                (do ; Send the packet to the specified input channel.
                  (>!! destination x)
                  (>!! destination y)))
              (recur packet-buffers ; And wait for the next available value.
                     (a/alts!! output-channels))))
          (recur packet-buffers (a/alts!! output-channels)))))))
