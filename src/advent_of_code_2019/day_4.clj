(ns advent-of-code-2019.day-4
  "Solutions to the Day 4 problems"
  (:require [clojure.repl :refer :all]))

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
