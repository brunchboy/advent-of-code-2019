(ns advent-of-code-2019.day-11
  "Solutions to the Day 11 problems."
  (:require [clojure.repl :refer :all]
            [clojure.core.async :as a :refer [>! <! >!! <!!]]
            [advent-of-code-2019.day-9 :refer [intcode-async]]))

(def program
  "The registration code painting intcode program."
  [3,8,1005,8,320,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,1,10,4,10,
   1001,8,0,29,2,101,10,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,101,0,8,54,2,3,16,10,3,8,
   1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,102,1,8,81,1006,0,75,3,8,1002,8,-1,10,1001,10,1,10,4,10,
   108,0,8,10,4,10,101,0,8,105,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1001,8,0,128,3,8,
   1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,102,1,8,149,1,105,5,10,1,105,20,10,3,8,102,-1,8,10,
   101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,179,1,101,1,10,2,109,8,10,1006,0,74,3,8,1002,8,-1,10,
   101,1,10,10,4,10,1008,8,1,10,4,10,1001,8,0,213,1006,0,60,2,1105,9,10,1,1005,11,10,3,8,1002,8,-1,10,
   101,1,10,10,4,10,108,1,8,10,4,10,1002,8,1,245,1,6,20,10,1,1103,11,10,2,6,11,10,2,1103,0,10,3,8,
   1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,1002,8,1,284,2,1103,12,10,2,1104,14,10,2,
   1004,12,10,2,1009,4,10,101,1,9,9,1007,9,968,10,1005,10,15,99,109,642,104,0,104,1,
   21102,1,48063419288,1,21102,1,337,0,1105,1,441,21101,0,846927340300,1,21101,0,348,0,
   1105,1,441,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,
   104,0,104,0,3,10,104,0,104,1,21102,1,235245104151,1,21102,395,1,0,1105,1,441,21102,29032123584,1,1,
   21101,0,406,0,1105,1,441,3,10,104,0,104,0,3,10,104,0,104,0,21101,0,709047878500,1,21101,429,0,0,
   1106,0,441,21101,868402070284,0,1,21102,1,440,0,1105,1,441,99,109,2,22102,1,-1,1,21101,40,0,2,
   21101,0,472,3,21102,462,1,0,1105,1,505,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,467,468,483,4,0,
   1001,467,1,467,108,4,467,10,1006,10,499,1102,1,0,467,109,-2,2106,0,0,0,109,4,2101,0,-1,504,1207,-3,0,10,
   1006,10,522,21101,0,0,-3,22101,0,-3,1,21202,-2,1,2,21101,1,0,3,21102,541,1,0,1106,0,546,109,-4,
   2106,0,0,109,5,1207,-3,1,10,1006,10,569,2207,-4,-2,10,1006,10,569,21202,-4,1,-4,1105,1,637,
   22102,1,-4,1,21201,-3,-1,2,21202,-2,2,3,21101,588,0,0,1105,1,546,22102,1,1,-4,21101,0,1,-1,
   2207,-4,-2,10,1006,10,607,21101,0,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,629,21201,-1,0,1,
   21102,629,1,0,106,0,504,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0])

(defn turn
  "Given a current direction and a turn code returned by the painting
  program, return the new direction to face."
  [direction code]
  (case direction
    :north (case code 0 :west 1 :east)
    :east  (case code 0 :north 1 :south)
    :south (case code 0 :east 1 :west)
    :west  (case code 0 :south :north)))

(defn move
  "Given a current position and a direction to move, compute the
  resulting position."
  [[x y] direction]
  (case direction
    :north [x (dec y)]
    :east  [(inc x) y]
    :south [x (inc y)]
    :west  [(dec x) y]))

(defn paint-panels
  "Set up the intcode program that controls a painting robot, and return
  the squares it paints. For part 2, added the option of setting the
  initial panel color reported to the robot."
  ([]
   (paint-panels 0))
  ([initial-color]
   (let [input-chan  (a/chan 5)
         output-chan (a/chan 5)
         robot       (intcode-async program input-chan output-chan)]
     (loop [panels    {[0 0] initial-color}
            position  [0 0]
            direction :north]
       (>!! input-chan (get panels position 0))
       (if-let [color  (<!! output-chan)]
         (let [direction (turn direction (<!! output-chan))]
           (recur (assoc panels position color)
                  (move position direction)
                  direction))
         panels)))))

;; Part 2

(defn draw-panels
  "Run the painting program starting on a white panel, then create an
  ASCII art representation of the resulting painted panels."
  []
  (let [panels (paint-panels 1)
        min-x  (apply min (map first (keys (paint-panels))))
        max-x  (apply max (map first (keys (paint-panels))))
        min-y  (apply min (map second (keys (paint-panels))))
        max-y  (apply max (map second (keys (paint-panels))))
        width  (- max-x min-x)
        height (- max-y min-y)]
    (loop [grid     (vec (for [_ (range height)] (vec (repeat width \.))))
           to-paint (keys panels)]
      (if-let [[x y] (first to-paint)]
        (recur (assoc-in grid [(- y min-y) (- x min-x)] (if (zero? (get panels [x y])) \. \#))
               (rest to-paint))
        (vec (map #(apply str %) grid))))))

(defn draw-easter-egg
  "Run the painting program stating on a black panel, and graphically
  render the results to make them easier to see."
  []
  (let [panels (paint-panels 0)
        min-x  (apply min (map first (keys (paint-panels))))
        max-x  (apply max (map first (keys (paint-panels))))
        min-y  (apply min (map second (keys (paint-panels))))
        max-y  (apply max (map second (keys (paint-panels))))
        width  (- max-x min-x)
        height (- max-y min-y)
        translate (fn [[x y]] [(- x min-x) (- y min-y)])]
    (doto (javax.swing.JFrame. "Painted Panels")
      (.setContentPane
       (doto (proxy [javax.swing.JPanel] []
               (paintComponent [^java.awt.Graphics g]
                 (let [g (doto ^java.awt.Graphics2D (.create g)
                           (.setPaint java.awt.Color/BLACK)
                           (.fill (java.awt.geom.Rectangle2D$Double. 0.0 0.0 width height))
                           (.setStroke (java.awt.BasicStroke. 1))
                           (.setPaint java.awt.Color/WHITE))
                       draw (fn [[x y]]
                              (.fill g (java.awt.geom.Rectangle2D$Double. x y 1.0 1.0)))]
                   (doseq [[k v] panels]
                     (when (pos? v) (draw (translate k)))))))
         (.setPreferredSize (java.awt.Dimension. width height))))
      (.pack)
      (.setVisible true))))
