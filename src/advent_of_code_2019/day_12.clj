(ns advent-of-code-2019.day-12
  "Solutions to the Day 12 problems."
  (:require [clojure.repl :refer :all]))

(def initial-state
  "The starting positions of the moons. In problem statement:
  <x=4, y=12, z=13>
  <x=-9, y=14, z=-3>
  <x=-7, y=-1, z=2>
  <x=-11, y=17, z=-1>"
  [[4 12 13 0 0 0]
   [-9 14 -3 0 0 0]
   [-7 -1 2 0 0 0]
   [-11 17 -1 0 0 0]])

(defn signum
  "A quick and dirty long-based implementation of signum, which returns
  the sign of a number."
  [n]
  (if (zero? n) 0 (if (neg? n) -1 1)))

(defn gravity-deltas
  "Calculates the changes in velocity that will take effect in each
  dimension due to the relative position of two moons."
  [subject other]
  (let [[s-x s-y s-z] subject
        [o-x o-y o-z] other]
    [(signum (- o-x s-x)) (signum (- o-y s-y)) (signum (- o-z s-z))]))

(defn gravity-step
  "Updates the system state for a single step of the simplified notion
  of gravity."
  [moons]
  (let [moons-set (set moons)]
    (vec (for [moon moons]
           (let [[x y z vx vy vz] moon
                 deltas (map (partial gravity-deltas moon) (clojure.set/difference moons-set #{moon}))
                 result (apply map + (concat deltas [[vx vy vz]]))]
             (vec (concat [x y z] result)))))))

(defn step
  "Computes a step in the evolution of the system, first applying
  gravity to update the velocities of the moons, and then moving them
  based on the resulting velocity."
  [moons]
  (vec (for [moon (gravity-step moons)]
         (vec (concat (apply map + (partition 3 moon)) (drop 3 moon))))))

(defn moon-energy
  "Calculates the energy of a single moon."
  [moon]
  (let [[position velocity] (partition 3 (map #(Math/abs %) moon))
        potential           (apply + position)
        kinetic             (apply + velocity)]
    (* potential kinetic)))

(defn energy
  "Calculates the problem's notion of the system's energy."
  [moons]
  (apply + (map moon-energy moons)))

(defn extract-axis-pairs
  "Pull out all the numbers that affect a single spatial axis from the
  current system state."
  [moons axis]
  (map (fn [moon] [(nth moon axis) (nth moon (+ axis 3))]) moons))

(defn record-period-when-first-seen
  "Given a single-axis state snapshot of the system, check if we have
  ever seen that state before, and if so, record the current number of
  steps as the period of oscillation in that axis. If we already have
  found a period for this axis, just leave it alone."
  [steps axis-state [seen period]]
  (if period
    [seen period]
    (if (seen axis-state)
      [seen steps]
      [(conj seen axis-state) period])))

(defn gcd
  "Calculate the greatest common divisor of two numbers."
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm
  "Calculate the least common multiple of two numbers."
  [a b]
  (/ (* a b) (gcd a b)))

(defn steps-until-match
  "Solve part 2 by finding how long the cycles are in each of the three
  axes, and then calculating the least common multiple of them."
  [moons]
  (let [periods (loop [steps           0
                       state           moons
                       seen-and-period [[#{} nil] [#{} nil] [#{} nil]]]
                  (when (zero? (mod steps 10000)) (println (format "%,12d steps..." steps)))
                  (if (= 3 (count (filter identity (map second seen-and-period))))
                    (vec (map second seen-and-period))
                    (recur (inc steps)
                           (step state)
                           (let [x-state (extract-axis-pairs state 0)
                                 y-state (extract-axis-pairs state 1)
                                 z-state (extract-axis-pairs state 2)
                                 x-sap   (first seen-and-period)
                                 y-sap   (second seen-and-period)
                                 z-sap   (nth seen-and-period 2)]
                             [(record-period-when-first-seen steps x-state x-sap)
                              (record-period-when-first-seen steps y-state y-sap)
                              (record-period-when-first-seen steps z-state z-sap)]))))]
    (reduce lcm periods)))
