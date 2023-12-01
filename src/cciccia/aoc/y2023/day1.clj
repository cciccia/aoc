(ns cciccia.aoc.y2023.day1
  (:require [cciccia.aoc.utils :as utils]
            [clojure.pprint :as pprint]
            [clojure.string :as str]))



(defn part1
  [input]
  (let [lines (utils/lined-spaced-input->str input)]
    (apply + (map
               (fn [line]
                 (->> ((juxt first last) (re-seq #"[1-9]" line))
                      (str/join "")
                      (Integer/parseInt)))
               lines))))

(defn transform-to-number
  [n]
  (get {"one" "1"
        "two" "2"
        "three" "3"
        "four" "4"
        "five" "5"
        "six" "6"
        "seven" "7"
        "eight" "8"
        "nine" "9"} n n))

(defn part2
  [input]
  (let [lines (utils/lined-spaced-input->str input)]
    (apply + (map
               (fn [line]
                 (->> (map second (re-seq #"(?=([0-9]|one|two|three|four|five|six|seven|eight|nine))" line))
                      ((juxt first last))
                      (map transform-to-number)
                      (str/join "")
                      (Integer/parseInt)))
               lines))))


(comment
  (part1 "2023/day1.txt")
  (part2 "2023/day1.txt"))