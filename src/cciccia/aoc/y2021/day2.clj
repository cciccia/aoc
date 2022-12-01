(ns cciccia.aoc.y2021.day2
  (:require [cciccia.aoc.utils :as utils]))

(defn part1
  [input]
  (let [{:keys [pos depth]} (reduce
                              (fn [{:keys [pos depth]} [dir mag]]
                                (case dir
                                  "forward"
                                  {:pos (+ pos mag)
                                   :depth depth}

                                  "down"
                                  {:pos pos
                                   :depth (+ depth mag)}

                                  "up"
                                  {:pos pos
                                   :depth (- depth mag)}))
                              {:pos 0 :depth 0}
                              input)]
    (* pos depth)))

(defn part2
  [input]
  (let [{:keys [aim pos depth]} (reduce
                                  (fn [{:keys [aim pos depth]} [dir mag]]
                                    (case dir
                                      "forward"
                                      {:aim aim
                                       :pos (+ pos mag)
                                       :depth (+ depth (* aim mag))}

                                      "down"
                                      {:aim (+ aim mag)
                                       :pos pos
                                       :depth depth}

                                      "up"
                                      {:aim (- aim mag)
                                       :pos pos
                                       :depth depth}))
                                  {:aim 0 :pos 0 :depth 0}
                                  input)]
    (* pos depth)))

(comment
  (part1 (utils/load-edn-input "2021/day2.edn"))
  (part2 (utils/load-edn-input "2021/day2.edn")))

