(ns cciccia.aoc.y2020.day12
  (:require [cciccia.aoc.utils :as utils]))

(defn turn
  [current-direction turn-direction degrees]
  (get {["E" "L" 90]  "N"
        ["E" "L" 180] "W"
        ["E" "L" 270] "S"
        ["E" "R" 90]  "S"
        ["E" "R" 180] "W"
        ["E" "R" 270] "N"
        ["N" "L" 90]  "W"
        ["N" "L" 180] "S"
        ["N" "L" 270] "E"
        ["N" "R" 90]  "E"
        ["N" "R" 180] "S"
        ["N" "R" 270] "W"
        ["W" "L" 90]  "S"
        ["W" "L" 180] "E"
        ["W" "L" 270] "N"
        ["W" "R" 90]  "N"
        ["W" "R" 180] "E"
        ["W" "R" 270] "S"
        ["S" "L" 90]  "E"
        ["S" "L" 180] "N"
        ["S" "L" 270] "W"
        ["S" "R" 90]  "W"
        ["S" "R" 180] "N"
        ["S" "R" 270] "E"}
       [current-direction turn-direction degrees]))

(defn translate
  [[x y] direction amount]
  (case direction
    "E" [(+ x amount) y]
    "N" [x (+ y amount)]
    "W" [(- x amount) y]
    "S" [x (- y amount)]))

(defn part1
  [input]
  (loop [instructions input
         location     [0 0]
         direction    "E"]
    (let [[instruction amount] (first instructions)]
      (cond
        (nil? instruction)
        (+ (Math/abs (first location))
           (Math/abs (second location)))

        (contains? #{"E" "N" "W" "S"} instruction)
        (recur (rest instructions) (translate location instruction amount) direction)

        (= "F" instruction)
        (recur (rest instructions) (translate location direction amount) direction)

        (contains? #{"L" "R"} instruction)
        (recur (rest instructions) location (turn direction instruction amount))))))

(defn rotate-waypoint-around-ship
  [[ship-x ship-y] [waypoint-x waypoint-y] direction amount]
  (let [diff-x (- waypoint-x ship-x)
        diff-y (- waypoint-y ship-y)]
    (condp contains?
           [direction amount]
      #{["L" 90] ["R" 270]}
      [(- ship-x diff-y) (+ ship-y diff-x)]

      #{["L" 270] ["R" 90]}
      [(+ ship-x diff-y) (- ship-y diff-x)]

      #{["L" 180] ["R" 180]}
      [(- ship-x diff-x) (- ship-y diff-y)])))

(defn move-ship-and-waypoint
  [[ship-x ship-y] [waypoint-x waypoint-y] repeat-factor]
  (let [diff-x (- waypoint-x ship-x)
        diff-y (- waypoint-y ship-y)]
    [[(+ ship-x (* repeat-factor diff-x)) (+ ship-y (* repeat-factor diff-y))]
     [(+ waypoint-x (* repeat-factor diff-x)) (+ waypoint-y (* repeat-factor diff-y))]]))


(defn part2
  [input]
  (loop [instructions      input
         ship-location     [0 0]
         waypoint-location [10 1]]
    (let [[instruction amount] (first instructions)]
      (cond
        (nil? instruction)
        (+ (Math/abs (first ship-location))
           (Math/abs (second ship-location)))

        (contains? #{"E" "N" "W" "S"} instruction)
        (recur (rest instructions) ship-location (translate waypoint-location instruction amount))

        (= "F" instruction)
        (let [[new-ship-location new-waypoint-location] (move-ship-and-waypoint ship-location waypoint-location amount)]
          (recur (rest instructions) new-ship-location new-waypoint-location))

        (contains? #{"L" "R"} instruction)
        (recur (rest instructions) ship-location (rotate-waypoint-around-ship ship-location waypoint-location instruction amount))))))

(comment
  (part1 (utils/load-edn-input "day12.edn")))

(comment
  (part2 [["F" 10]
          ["N" 3]
          ["F" 7]
          ["R" 90]
          ["F" 11]]))

(comment
  (part2 (utils/load-edn-input "day12.edn")))

