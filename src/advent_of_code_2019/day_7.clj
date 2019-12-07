(ns advent-of-code-2019.day-7
  "Solutions to the Day 7 problems"
  (:require [clojure.repl :refer :all]
            [clojure.math.combinatorics :as combo]
            [clojure.core.async :as a :refer [>! <! >!! <!!]]))

(def example-program-1
  "The first example program for testing my solution."
  [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0])

(def example-program-2
  "The second example program for testing my solution."
  [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0])

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
  [settings]
  (loop [result    0
         remaining settings]
    (if (seq remaining)
      (recur (first (intcode [(first remaining) result]))
             (rest remaining))
      result)))

(defn find-optimal-settings
  "Try all phase permutations and return the best, as well as its
  resulting thrust."
  []
  (loop [result    [[] 0]
         remaining (combo/permutations [0 1 2 3 4])]
    (if (seq remaining)
      (let [settings (first remaining)
            signal   (try-settings settings)
            best     (if (> signal (nth result 1))
                       [settings signal]
                       result)]
        (recur best (rest remaining)))
      result)))

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

(def example-async-program-1
  "The first sample program for testing the asynchronous feedback solution."
  [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])

(def example-async-program-2
  "The second sample program for testing the asynchronous feedback solution."
  [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
   -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
   53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10])

(defn async-try-settings
  "Run a chain of async amplifiers with the supplied phase settings,
  returning the final amplifier's output."
  [settings]
  ;; Set up the channels that will feed the output of one amplifier to
  ;; the input of the next, and tap the first amplifier's input so we
  ;; can track the values coming out.
  (let [channels        (mapv (fn [phase] (let [result (a/chan 5)]
                                            (>!! result phase)
                                            result))
                              settings)
        mult            (a/mult (first channels))
        tap             (a/tap mult (a/chan 5))
        tapped-channels (assoc channels 0 (a/tap mult (a/chan 5)))]
    (dotimes [i (count channels)]
      (intcode-async (nth tapped-channels i) (nth channels (mod (inc i) (count channels)))))
    (>!! (nth channels 0) 0)  ; Send seed value to first amplifier in the chain.
    (<!! (a/reduce (fn [_ latest] latest) nil tap))))  ; Block until the tap closes, returning the final value.

;; Note: There is some kind of race condition which sometimes causes
;; async-try-settings to never terminate. Interrupting and running
;; again can eventually get all the way through. I may want to try
;; switching to using pipelines to tie the channels together rather
;; than using a single channel as input and output to different
;; interpreters, but this worked well enough for now.

(defn async-find-optimal-settings
  "Try all phase permutations of the async amplifier configuration and
  return the best, as well as its resulting thrust."
  []
  (loop [result    [[] 0]
         remaining (combo/permutations [5 6 7 8 9])]
    (if (seq remaining)
      (let [settings (first remaining)
            signal   (async-try-settings settings)
            best     (if (> signal (nth result 1))
                       [settings signal]
                       result)]
        (println "best so far:" best)
        (recur best (rest remaining)))
      result)))
