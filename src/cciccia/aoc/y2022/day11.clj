(ns cciccia.aoc.y2022.day11
  (:require [cciccia.aoc.utils :as utils]))

(def stuff
  {0 {:items (utils/queue [77, 69, 76, 77, 50, 58])
      :op    #(* % 11)
      :test  #(if (zero? (mod % 5)) 1 5)}
   1 {:items (utils/queue [75, 70, 82, 83, 96, 64, 62])
      :op    #(+ % 8)
      :test  #(if (zero? (mod % 17)) 5 6)}
   2 {:items (utils/queue [53])
      :op    #(* % 3)
      :test  #(if (zero? (mod % 2)) 0 7)}
   3 {:items (utils/queue [85, 64, 93, 64, 99])
      :op    #(+ % 4)
      :test  #(if (zero? (mod % 7)) 7 2)}
   4 {:items (utils/queue [61, 92, 71])
      :op    #(* % %)
      :test  #(if (zero? (mod % 3)) 2 3)}
   5 {:items (utils/queue [79, 73, 50, 90])
      :op    #(+ % 2)
      :test  #(if (zero? (mod % 11)) 4 6)}
   6 {:items (utils/queue [50, 89])
      :op    #(+ % 3)
      :test  #(if (zero? (mod % 13)) 4 3)}
   7 {:items (utils/queue [83, 56, 64, 58, 93, 91, 56, 65])
      :op    #(+ % 5)
      :test  #(if (zero? (mod % 19)) 1 0)}})

(defn part1
  [input]
  (let [mb (loop [turn 0
                  round 1
                  system input
                  seen {}]
             (if-let [next-item (peek (get-in system [turn :items]))]
               (let [opped-item ((get-in system [turn :op]) next-item)
                     opped-2-item (int (Math/floor (/ opped-item 3)))
                     target ((get-in system [turn :test]) opped-2-item)]
                 (recur turn
                        round
                        (-> system
                            (update-in [turn :items] pop)
                            (update-in [target :items] conj opped-2-item))
                        (-> seen
                            (assoc turn (inc (get seen turn 0))))))
               (if-not (and (= turn 7)
                            (= round 20))
                 (recur (mod (inc turn) 8)
                        (cond-> round (= turn 7) inc)
                        system
                        seen)
                 seen)))]
    (->> mb
         vals
         sort
         reverse
         (take 2)
         (apply *))))


(defn part2
  [input]
  (let [mb (loop [turn 0
                  round 1
                  system input
                  seen {}]
             (if-let [next-item (peek (get-in system [turn :items]))]
               (let [opped-item ((get-in system [turn :op]) next-item)
                     opped-2-item (mod opped-item 9699690)
                     target ((get-in system [turn :test]) opped-2-item)]
                 (recur turn
                        round
                        (-> system
                            (update-in [turn :items] pop)
                            (update-in [target :items] conj opped-2-item))
                        (-> seen
                            (assoc turn (inc (get seen turn 0))))))
               (if-not (and (= turn 7)
                            (= round 10000))
                 (recur (mod (inc turn) 8)
                        (cond-> round (= turn 7) inc)
                        system
                        seen)
                 seen)))]
    (->> mb
         vals
         sort
         reverse
         (take 2)
         (apply *))))


(comment
  (part1 stuff)
  (part2 stuff))

