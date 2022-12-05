(ns cciccia.aoc.y2022.day5
  (:require [clojure.string :as str]
            [cciccia.aoc.utils :as utils]))

(defn part1
  [initial instructions reverse?]
  (let [stacks (->> initial
                    (map-indexed
                      (fn [i line]
                        [(inc i) (apply conj (list) (str/split line #""))]))
                    (into {}))
        new-stacks (reduce
                     (fn [acc [n from to]]
                       (let [[popped remaining] ((juxt (partial take n) (partial drop n)) (get acc from))]
                         (-> acc
                             (assoc from remaining)
                             (update to #(apply conj % (if reverse? popped (reverse popped)))))))
                     stacks
                     instructions)]
    (->> (map
           (fn [i]
             (first (get new-stacks (inc i))))
           (range 9))
         (str/join ""))))

(comment
  (part1 (utils/lined-spaced-input->str "2022/day5-initial.txt") (utils/lined-spaced-input->2d-vec-int "2022/day5-instructions.txt") true)
  (part1 (utils/lined-spaced-input->str "2022/day5-initial.txt") (utils/lined-spaced-input->2d-vec-int "2022/day5-instructions.txt") false))
