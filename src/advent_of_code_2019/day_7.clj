(ns advent-of-code-2019.day-7
  "Solutions to the Day 7 problems"
  (:require [clojure.repl :refer :all]
            [clojure.math.combinatorics :as combo]
            [clojure.core.async :as a :refer [>! <! >!! <!!]]))

(def program
  "The intcode program data for day 7 part 1"
  [3,8,1001,8,10,8,105,1,0,0,21,34,43,64,85,98,179,260,341,422,99999,3,9,1001,9,3,9,102,3,9,9,4,9,99,3,9,
   102,5,9,9,4,9,99,3,9,1001,9,2,9,1002,9,4,9,1001,9,3,9,1002,9,4,9,4,9,99,3,9,1001,9,3,9,102,3,9,9,
   101,4,9,9,102,3,9,9,4,9,99,3,9,101,2,9,9,1002,9,3,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,
   102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,
   1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,
   101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,
   101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,
   102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,
   1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,
   102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,
   101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,
   1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,
   1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,99])

(defn intcode
  "Run the day 7 intcode interpreter on the supplied program and input data, gathering output."
  ([input-data]
   (intcode program input-data))
  ([program-data input-data]
   (loop [i       0
          memory  program-data
          inputs  (seq input-data)
          outputs []]
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
                   (assoc memory (load-relative 3) (+ (resolve-operand 0) (resolve-operand 1)))
                   inputs outputs)
         2  (recur (+ i 4)  ; Multiply
                   (assoc memory (load-relative 3) (* (resolve-operand 0) (resolve-operand 1)))
                   inputs outputs)
         3  (recur (+ i 2)  ; Read
                   (assoc memory (load-relative 1) (first inputs))
                   (rest inputs) outputs)
         4  (recur (+ i 2)  ; Print
                   memory inputs (conj outputs (resolve-operand 0)))
         5  (recur (if (zero? (resolve-operand 0))  ; Jump if true
                     (+ i 3)
                     (resolve-operand 1))
                   memory inputs outputs)
         6  (recur (if (zero? (resolve-operand 0))  ; Jump if false
                     (resolve-operand 1)
                     (+ i 3))
                   memory inputs outputs)
         7  (recur (+ i 4)  ; Less than
                   (assoc memory (load-relative 3)
                          (if (< (resolve-operand 0) (resolve-operand 1)) 1 0))
                   inputs outputs)
         8  (recur (+ i 4)  ; Equals
                   (assoc memory (load-relative 3)
                          (if (= (resolve-operand 0) (resolve-operand 1)) 1 0))
                   inputs outputs)
         99 outputs)))))

(defn try-settings
  "Run a chain of amplifiers with the supplied phase settings,
  returning the final amplifier's output."
  [program-data settings]
  (loop [result    0
         remaining settings]
    (if (seq remaining)
      (recur (first (intcode program-data [(first remaining) result]))
             (rest remaining))
      result)))

(defn find-optimal-settings
  "Try all phase permutations and return the best, as well as its
  resulting thrust."
  ([]
   (find-optimal-settings program))
  ([program-data]
   (let [all-settings (combo/permutations [0 1 2 3 4])]
     (loop [result    [(vec (first all-settings)) (try-settings program-data (first all-settings))]
            remaining (rest all-settings)]
       (if (seq remaining)
         (let [settings (first remaining)
               signal   (try-settings program-data settings)
               best     (if (> signal (nth result 1))
                          [settings signal]
                          result)]
           (recur best (rest remaining)))
         result)))))

;; Part 2

(defn intcode-async
  "Asynchronous version of the day 7 intcode interpreter, using channels
  for input and output. Returns a channel that will be closed when the
  interpreter halts."
  ([input-chan output-chan]
   (intcode-async program input-chan output-chan))
  ([program-data input-chan output-chan]
   (a/go-loop [i       0
               memory  program-data]
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
       (case opcode
         1  (recur (+ i 4)  ; Add
                   (assoc memory (load-relative 3) (+ (resolve-operand 0) (resolve-operand 1))))
         2  (recur (+ i 4)  ; Multiply
                   (assoc memory (load-relative 3) (* (resolve-operand 0) (resolve-operand 1))))
         3  (recur (+ i 2)  ; Read
                   (assoc memory (load-relative 1) (<! input-chan)))
         4  (recur (+ i 2)  ; Print
                   (do
                     (>! output-chan (resolve-operand 0))
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
         99 (a/close! output-chan))))))

(defn async-try-settings
  "Run a chain of async amplifiers with the supplied phase settings,
  returning the final amplifier's output."
  ([settings]
   (async-try-settings program))
  ([program-data settings]
   ;; Set up the channels that will feed the output of one amplifier to
   ;; the input of the next, and a final output channel that the main
   ;; loop will use to track values coming out and feed them back in to
   ;; the first amplifier.
   (let [input-channels (mapv (fn [phase] (let [result (a/chan 5)]
                                            (>!! result phase)
                                            result))
                              settings)
         first-input    (nth input-channels 0)
         last-output    (a/chan 5)]
     (dotimes [i (count input-channels)]
       (intcode-async program-data (nth input-channels i) (nth input-channels (inc i) last-output)))
     (>!! first-input 0)  ; Send seed value to first amplifier in the chain.
     ;; Run the feedback loop until the last amplifier shuts down, returning its final output value.
     (loop [result nil
            latest (<!! last-output)]
       (if latest
         (do
           (>!! first-input latest)
           (recur latest (<!! last-output)))
         result)))))

(defn async-find-optimal-settings
  "Try all phase permutations of the async amplifier configuration and
  return the best, as well as its resulting thrust."
  ([]
   (async-find-optimal-settings program))
  ([program-data]
   (loop [result    [[] 0]
          remaining (combo/permutations [5 6 7 8 9])]
     (if (seq remaining)
       (let [settings (first remaining)
             signal   (async-try-settings program-data settings)
             best     (if (> signal (nth result 1))
                        [settings signal]
                        result)]
         #_(println "best so far:" best)
         (recur best (rest remaining)))
       result))))
