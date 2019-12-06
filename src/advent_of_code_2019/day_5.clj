(ns advent-of-code-2019.day-5
  "Solutions to the Day 5 problems"
  (:require [clojure.repl :refer :all]))

(def program
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

(defn intcode
  "Run the day 5 intcode interpreter on the input data."
  ([]
   (intcode program))
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
