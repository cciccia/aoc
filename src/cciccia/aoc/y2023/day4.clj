(ns cciccia.aoc.y2023.day4
  (:require [cciccia.aoc.utils :as utils]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn part1
  [input]
  (->> input
       (map (fn [line]
              (let [[_ _card-id other] (re-matches #"Card +(\d+):(.*)" line)
                    [winning-str mine-str] (str/split other #" \|")
                    winning (map #(Integer/parseInt (str/trim (apply str %))) (partition 3 winning-str))
                    mine (map #(Integer/parseInt (str/trim (apply str %))) (partition 3 mine-str))
                    matches (count (set/intersection (set winning) (set mine)))]
                (if (pos? matches)
                  (int (Math/pow 2 (dec matches)))
                  0))))
       (apply +)))

(defn part2
  [input]
  (let [key (->> input
                 (map (fn [line]
                        (let [[_ card-id other] (re-matches #"Card +(\d+):(.*)" line)
                              [winning-str mine-str] (str/split other #" \|")
                              winning (map #(Integer/parseInt (str/trim (apply str %))) (partition 3 winning-str))
                              mine (map #(Integer/parseInt (str/trim (apply str %))) (partition 3 mine-str))
                              matches (count (set/intersection (set winning) (set mine)))]
                          [(Integer/parseInt card-id) matches])))
                 (into {}))]
    (->> (sort-by first key)
         (reduce (fn [cards [card-id matches]]
                   (reduce (fn [acc2 i]
                             (update acc2 (+ card-id i) + (get cards card-id)))
                           cards
                           (range 1 (inc matches))))
                 (into {} (map (fn [x y] [x y]) (keys key) (repeat 1))))
         ((fn [d]
            (println (sort-by first d)) d))
         vals
         (apply +))))


(comment
  (part1 (utils/lined-spaced-input->str "2023/day4.txt"))
  (part2 (utils/lined-spaced-input->str "2023/day4-sample.txt")))


