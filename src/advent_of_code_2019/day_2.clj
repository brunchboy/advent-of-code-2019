(ns advent-of-code-2019.day-2
  "Solutions to the Day 2 problems"
  (:require [clojure.repl :refer :all]))

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

(defn intcode
  "Run the intcode interpreter on the input data."
  ([]
   (intcode program))
  ([data]
   (loop [i      0
          memory data]
     (let [opcode (nth memory i)
           op1    (nth memory (inc i) 0)
           op2    (nth memory (+ i 2) 0)
           dest   (nth memory (+ i 3) 0)]
       #_(println "i:" i "opcode:" opcode "op1:" op1 "op2" op2 "dest:" dest)
       (case opcode
         1  (recur (+ i 4)
                   (assoc memory dest (+ (nth memory op1) (nth memory op2))))
         2  (recur (+ i 4)
                   (assoc memory dest (* (nth memory op1) (nth memory op2))))
         99 memory)))))

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
