(ns cciccia.aoc.y2022.day10
  (:require [cciccia.aoc.utils :as utils]))

(defn get-vals
  [input]
  (loop [ctr   1
         reg   1
         vals  {}
         input input]
    (let [[[instr num] & others] input]
      (if instr
        (case instr
          "noop"
          (recur (inc ctr)
                 reg
                 (assoc vals ctr reg)
                 others)

          "addx"
          (recur (+ ctr 2)
                 (+ reg num)
                 (-> vals
                     (assoc ctr reg)
                     (assoc (inc ctr) reg))
                 others))
        vals))))

(defn part1
  [input]
  (let [vals (get-vals input)]
    (+ (* 20 (get vals 20))
       (* 60 (get vals 60))
       (* 100 (get vals 100))
       (* 140 (get vals 140))
       (* 180 (get vals 180))
       (* 220 (get vals 220)))))

(defn part2
  [input]
  (let [vals (get-vals input)]
    (loop [ctr 1]
      (let [sprite (get vals ctr)
            drawn  (mod (dec ctr) 40)]
        (if (or (= sprite drawn)
                (= (inc sprite) drawn)
                (= (dec sprite) drawn))
          (print "#")
          (print "."))
        (when (zero? (mod ctr 40))
          (print "\n"))
        (when (not= 240 ctr)
          (recur (inc ctr)))))))


(comment
  (part1 (utils/load-edn-input "2022/day10.edn"))
  (part2 (utils/load-edn-input "2022/day10.edn")))
