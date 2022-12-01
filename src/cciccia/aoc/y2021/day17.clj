(ns cciccia.aoc.y2021.day17
  (:require [clojure.math.combinatorics :as comb]))

(defn x-step
  [{:keys [pos vel]}]
  {:pos (+ pos vel)
   :vel (cond
          (< vel 0)
          (inc vel)

          (> vel 0)
          (dec vel)

          :else
          0)})


(defn y-step
  [{:keys [pos vel]}]
  {:pos (+ pos vel)
   :vel (dec vel)})

(defn part1
  [y1 y2]
  (reduce
    (fn [p c]
      (let [next (reduce
                   (fn [seen traj]
                     (if (< (:pos traj) y1)
                       (if (and (contains? seen 0) (some #(<= y1 % y2) seen))
                         (do
                           (reduced (apply max seen)))
                         (reduced nil))
                       (conj seen (:pos traj))))
                   #{}
                   (drop 1 (iterate y-step {:pos 0 :vel c})))]
        (if next
          next
          p)))
    0
    (range 1000)))

(defn part2
  [p1 p2 step-fn x?]
  (reduce
    (fn [p c]
      (let [next (reduce
                   (fn [seen traj]
                     (if (or (and (< (:pos traj) p1) (not x?))
                             (and (zero? (:vel traj)) x?))
                       (if (some #(<= p1 % p2) seen)
                         (reduced c)
                         (reduced nil))
                       (conj seen (:pos traj))))
                   #{}
                   (drop 1 (iterate step-fn {:pos 0 :vel c})))]
        (if next
          (conj p next)
          p)))
    #{}
    (range -1000 1000)))

(defn step
  [{:keys [pos vel]}]
  (let [[x-pos y-pos] pos
        [x-vel y-vel] vel
        next-x (x-step {:pos x-pos :vel x-vel})
        next-y (y-step {:pos y-pos :vel y-vel})]
    {:pos [(:pos next-x) (:pos next-y)]
     :vel [(:vel next-x) (:vel next-y)]}))


(defn part2-filter
  [x1 x2 y1 y2 possible-vels]
  (->> possible-vels
       (filter
         (fn [[xv yv]]
           (reduce
             (fn [seen traj]
               (if (and (< (second (:pos traj)) y1))
                 (reduced (some #(and (<= x1 (% 0) x2)
                                      (<= y1 (% 1) y2))
                                seen))
                 (conj seen (:pos traj))))
             #{}
             (iterate step {:pos [0 0] :vel [xv yv]}))))
       (count)))

(comment
  (part1 -74 -54)
  (part2 281 311 x-step true)
  (part2 -74 -54 y-step false)
  (part2-filter 281 311 -74 -54 (comb/cartesian-product (part2 281 311 x-step true) (part2 -74 -54 y-step false)))
  (part2-filter 20 30 -10 -5 (comb/cartesian-product (part2 20 30 x-step true) (part2 -10 -5 y-step false))))
