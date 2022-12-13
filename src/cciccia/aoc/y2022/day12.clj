(ns cciccia.aoc.y2022.day12
  (:require [cciccia.aoc.utils :as utils]))

(defn elevation
  [s]
  (let [chr (first s)]
    (cond
      (nil? chr) 124
      (= chr \S) 97
      (= chr \E) 122
      :else (int chr))))

(defn next-moves
  [grid distances visited [x y]]
  (let [cur      (get grid [x y])
        distance (get distances [x y])
        left     [(dec x) y]
        right    [(inc x) y]
        up       [x (dec y)]
        down     [x (inc y)]]
    (cond-> distances
            (and (not (contains? visited left)) (<= (elevation (get grid left)) (inc (elevation cur))))
            (assoc left (min (get distances left 99999) (inc distance)))
            (and (not (contains? visited right)) (<= (elevation (get grid right)) (inc (elevation cur))))
            (assoc right (min (get distances right 99999) (inc distance)))
            (and (not (contains? visited up)) (<= (elevation (get grid up)) (inc (elevation cur))))
            (assoc up (min (get distances up 99999) (inc distance)))
            (and (not (contains? visited down)) (<= (elevation (get grid down)) (inc (elevation cur))))
            (assoc down (min (get distances down 99999) (inc distance))))))

(defn part1
  [grid any?]
  (let [start (some
                (fn [[k v]]
                  (when (= v "S")
                    k))
                grid)
        target (some
                 (fn [[k v]]
                   (when (= v "E")
                     k))
                 grid)
        all-a (keep
                (fn [[k v]]
                  (when (= (elevation v) 97)
                    k))
                grid)]
    (-> (loop [distances (if any?
                           (into {} (map #(vector % 0) all-a))
                           {start 0})
               visited   #{}
               loc       start]
          (if (= target loc)
            (get distances loc)
            (let [distances (next-moves grid distances visited loc)
                  visited   (conj visited loc)]
              (recur distances
                     visited
                     (->> grid
                          (remove (fn [[k _v]]
                                    (contains? visited k)))
                          (sort-by (fn [[k _v]]
                                     (get distances k 99999)))
                          (ffirst)))))))))

(comment
  (part1 (-> (utils/lined-spaced-input->2d-vec-str "2022/day12.txt" #"")
             (utils/two-d-grid->map))
         true))
