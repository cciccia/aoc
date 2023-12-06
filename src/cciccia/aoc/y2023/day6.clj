(ns cciccia.aoc.y2023.day6)

(defn calc-distance
  [time delay]
  (* delay (- time delay)))

(defn part1
  [races]
  (->> races
       (map (fn [[time distance]]
              (->> (range 1 time)
                   (filter (fn [delay]
                             (> (calc-distance time delay) distance)))
                   (count))))
       (apply *)))


(comment
  (part1 [[62 644] [73 1023] [75 1240] [65 1023]])
  (time (part1 [[62737565 644102312401023]]))
  ("use quadradtic forumla lol a=-1 b=time c=-distance"))

