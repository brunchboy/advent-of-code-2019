(ns advent-of-code-2019.day-9
  "Solutions to the Day 9 problems"
  (:require [clojure.repl :refer :all]
            [clojure.core.async :as a :refer [>! <! >!! <!!]]))

(def program
  "The problem's program code."
  [1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,3,0,1000,109,988,209,12,9,1000,209,6,
   209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,
   1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1102,35,1,1010,1102,1,33,1013,1101,0,715,
   1022,1102,1,20,1004,1102,1,24,1012,1101,36,0,1005,1101,0,655,1024,1102,32,1,1014,1101,0,499,1026,
   1102,1,242,1029,1101,0,25,1002,1101,0,27,1017,1101,708,0,1023,1101,0,21,1016,1101,0,28,1000,
   1101,0,492,1027,1102,34,1,1015,1102,29,1,1007,1102,247,1,1028,1101,0,39,1011,1102,1,31,1018,
   1102,1,0,1020,1102,1,37,1006,1101,1,0,1021,1102,26,1,1009,1102,1,38,1008,1101,30,0,1019,1102,1,23,
   1001,1102,650,1,1025,1101,22,0,1003,109,7,2101,0,-7,63,1008,63,29,63,1005,63,205,1001,64,1,64,
   1105,1,207,4,187,1002,64,2,64,109,-1,1202,-1,1,63,1008,63,35,63,1005,63,227,1106,0,233,4,213,
   1001,64,1,64,1002,64,2,64,109,17,2106,0,5,4,239,1105,1,251,1001,64,1,64,1002,64,2,64,
   109,-1,21108,40,39,-4,1005,1018,271,1001,64,1,64,1106,0,273,4,257,1002,64,2,64,109,-9,
   1206,8,285,1106,0,291,4,279,1001,64,1,64,1002,64,2,64,109,-13,2108,27,0,63,1005,63,307,
   1106,0,313,4,297,1001,64,1,64,1002,64,2,64,109,11,2101,0,-5,63,1008,63,37,63,1005,63,339,4,319,
   1001,64,1,64,1105,1,339,1002,64,2,64,109,13,21101,41,0,-9,1008,1015,41,63,1005,63,365,4,345,
   1001,64,1,64,1106,0,365,1002,64,2,64,109,-14,1201,-6,0,63,1008,63,22,63,1005,63,385,1106,0,391,4,371,
   1001,64,1,64,1002,64,2,64,109,-10,1202,3,1,63,1008,63,22,63,1005,63,417,4,397,1001,64,1,64,1105,1,417,
   1002,64,2,64,109,6,1207,-3,21,63,1005,63,437,1001,64,1,64,1105,1,439,4,423,1002,64,2,64,
   109,16,21107,42,41,-8,1005,1014,455,1105,1,461,4,445,1001,64,1,64,1002,64,2,64,109,-28,2107,24,7,63,
   1005,63,481,1001,64,1,64,1106,0,483,4,467,1002,64,2,64,109,33,2106,0,0,1001,64,1,64,1106,0,501,4,489,
   1002,64,2,64,109,-18,2108,38,-1,63,1005,63,519,4,507,1105,1,523,1001,64,1,64,1002,64,2,64,109,-3,
   1208,-4,25,63,1005,63,545,4,529,1001,64,1,64,1106,0,545,1002,64,2,64,109,12,21102,43,1,-8,
   1008,1010,43,63,1005,63,571,4,551,1001,64,1,64,1106,0,571,1002,64,2,64,109,-1,1207,-8,27,63,
   1005,63,593,4,577,1001,64,1,64,1106,0,593,1002,64,2,64,109,-7,21101,44,0,8,1008,1018,42,63,
   1005,63,617,1001,64,1,64,1105,1,619,4,599,1002,64,2,64,109,-4,1208,-1,39,63,1005,63,639,
   1001,64,1,64,1105,1,641,4,625,1002,64,2,64,109,13,2105,1,5,4,647,1106,0,659,1001,64,1,64,
   1002,64,2,64,109,4,1206,-3,673,4,665,1106,0,677,1001,64,1,64,1002,64,2,64,109,-22,21108,45,45,10,
   1005,1011,699,4,683,1001,64,1,64,1105,1,699,1002,64,2,64,109,29,2105,1,-7,1001,64,1,64,
   1105,1,717,4,705,1002,64,2,64,109,-19,21107,46,47,5,1005,1016,739,4,723,1001,64,1,64,1106,0,739,
   1002,64,2,64,109,-8,2102,1,2,63,1008,63,33,63,1005,63,763,1001,64,1,64,1106,0,765,4,745,
   1002,64,2,64,109,1,1201,-2,0,63,1008,63,25,63,1005,63,791,4,771,1001,64,1,64,1105,1,791,
   1002,64,2,64,109,16,1205,0,803,1105,1,809,4,797,1001,64,1,64,1002,64,2,64,109,-8,1205,9,827,4,815,
   1001,64,1,64,1106,0,827,1002,64,2,64,109,-4,2102,1,-3,63,1008,63,36,63,1005,63,853,4,833,
   1001,64,1,64,1106,0,853,1002,64,2,64,109,17,21102,47,1,-6,1008,1019,50,63,1005,63,877,
   1001,64,1,64,1105,1,879,4,859,1002,64,2,64,109,-29,2107,22,5,63,1005,63,897,4,885,1106,0,901,
   1001,64,1,64,4,64,99,21102,27,1,1,21101,0,915,0,1106,0,922,21201,1,25338,1,204,1,99,109,3,
   1207,-2,3,63,1005,63,964,21201,-2,-1,1,21101,942,0,0,1105,1,922,22102,1,1,-1,21201,-2,-3,1,
   21102,957,1,0,1106,0,922,22201,1,-1,-2,1105,1,968,21202,-2,1,-2,109,-3,2106,0,0])

(defn assoc-growing
  "An extension of `assoc` that grows the memory to be large enough to
  hold the indexed value when needed."
  [memory i val]
  (let [expanded (apply conj memory (repeat (- i (count memory)) 0))]
    (assoc expanded i val)))

(defn intcode-async
  "Extended version of the day 7 asynchronous intcode interpreter,
  adding support for relative mode operands."
  ([input-chan output-chan]
   (intcode-async program input-chan output-chan))
  ([program-data input-chan output-chan]
   (a/go-loop [pc       0  ; Program counter: address of next instruction to be executed.
               rel-base 0  ; Relative addressing mode base address.
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
         1  (recur (+ pc 4) rel-base
                   (assoc-growing memory (resolve-address 2) (+ (resolve-operand 0) (resolve-operand 1))))

         ;; Multiply operand 0 by operand 1, storing the result in address specified by operand 2.
         2  (recur (+ pc 4) rel-base
                   (assoc-growing memory (resolve-address 2) (* (resolve-operand 0) (resolve-operand 1))))

         ;; Read an input value, storing it in the address specified by operand 0.
         3  (recur (+ pc 2) rel-base
                   (assoc-growing memory (resolve-address 0) (<! input-chan)))

         ;; Output the value specified by operand 0.
         4  (recur (+ pc 2) rel-base
                   (do
                     (>! output-chan (resolve-operand 0))
                     memory))

         ;; If the value of operand 0 is true (non-zero) jump to the address found in operand 1.
         5  (recur (if (zero? (resolve-operand 0))
                     (+ pc 3)
                     (resolve-operand 1))
                   rel-base
                   memory)

         ;; If the value of operand 0 is false (zero) jump to the address found in operand 1.
         6  (recur (if (zero? (resolve-operand 0))
                     (resolve-operand 1)
                     (+ pc 3))
                   rel-base
                   memory)

         ;; Store a 1 in the address specified by operand 2 if operand 0 is less than operand 1, otherwise store a 0.
         7  (recur (+ pc 4) rel-base
                   (assoc-growing memory (resolve-address 2)
                                  (if (< (resolve-operand 0) (resolve-operand 1)) 1 0)))

         ;; Store a 1 in the address specified by operand 2 if operand 0 equals operand 1, otherwise store a 0.
         8  (recur (+ pc 4) rel-base
                   (assoc-growing  memory (resolve-address 2)
                                   (if (= (resolve-operand 0) (resolve-operand 1)) 1 0)))

         ;; Add the value of operand zero to the relative address base.
         9  (recur (+ pc 2) (+ rel-base (resolve-operand 0)) memory)

         ;; End the program, closing the output channel.
         99 (a/close! output-chan))))))

(defn intcode
  "Synchronous wrapper for the async interpreter for simple cases,
  accepting and returning vectors of input and output."
  ([input-data]
   (intcode program input-data))
  ([program-data input-data]
   (let [in-chan  (a/chan 5)
         out-chan (a/chan 5)]
     (a/onto-chan in-chan input-data)
     (intcode-async program-data in-chan out-chan)
     (<!! (a/into [] out-chan)))))
