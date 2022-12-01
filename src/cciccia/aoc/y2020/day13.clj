(ns cciccia.aoc.y2020.day13)

(def input [29,37,409,17,13,19,23,353,41])

(defn part1
  [input target]
  (some
    (fn [n]
      (some #(when (zero? (mod n %))
               (* (- n target) %))
            input))
    (iterate inc target)))

(comment
  (part1 input 1000511))

(def input2 [[409 29] [353 60] [41 101] [37 23] [29 0] [23 52] [19 48] [17 46] [13 47]])

(defn part2
  [input start]
  (loop [remaining-input input
         n start
         step 1]
    (let [[test offset] (first remaining-input)]
      (cond
        (nil? test)
        n

        (zero? (mod (+ n (mod offset test)) test))
        (recur (rest remaining-input) n (* step test))

        :else
        (recur remaining-input (+ n step) step)))))

(comment
  (part2 input2 100000000000000))
