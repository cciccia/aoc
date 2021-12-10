(ns cciccia.aoc.2021.day10
  (:require [cciccia.aoc.utils :as utils]))

(defn line->bad-score
  [line]
  (loop [rest-of-line line
         stack (list)]
    (let [c (first rest-of-line)]
      (case c
        nil
        0

        (\( \[ \{ \<)
        (recur (rest rest-of-line) (conj stack c))

        (\) \] \} \>)
        (let [[d new-stack] ((juxt peek pop) stack)]
          (cond
            (or (and (= c \)) (= d \())
                (and (= c \]) (= d \[))
                (and (= c \}) (= d \{))
                (and (= c \>) (= d \<)))
            (recur (rest rest-of-line) new-stack)

            (= c \))
            3

            (= c \])
            57

            (= c \})
            1197

            (= c \>)
            25137))))))

(defn part1
  [input]
  (->> (map line->bad-score input)
       (apply +)))

(defn line->good-score
  [line]
  (loop [rest-of-line line
         stack (list)]
    (let [c (first rest-of-line)]
      (case c
        nil
        (reduce
          (fn [p c]
            (case c
              \(
              (+ 1 (* p 5))

              \[
              (+ 2 (* p 5))

              \{
              (+ 3 (* p 5))

              \<
              (+ 4 (* p 5))))
          0
          stack)

        (\( \[ \{ \<)
        (recur (rest rest-of-line) (conj stack c))

        (\) \] \} \>)
        (recur (rest rest-of-line) (pop stack))))))

(defn part2
  [input]
  (let [scores (->> (filter #(zero? (line->bad-score %)) input)
                    (map line->good-score)
                    sort)
        middle (long (Math/floor (/ (count scores) 2)))]
    (first (drop-last middle (drop middle scores)))))

(comment
  (part1 (utils/load-edn-input "2021/day10.edn"))
  (part2 (utils/load-edn-input "2021/day10.edn")))