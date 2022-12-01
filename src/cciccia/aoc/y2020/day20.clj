(ns cciccia.aoc.y2020.day20
  (:require [clojure.set :as set]
            [cciccia.aoc.utils :as utils]
            [taoensso.timbre :as timbre]
            [clojure.math.combinatorics :as combo]))

(defn do-nothing
  [_size [x y]]
  [x y])

(defn flip-horizontal
  [size [x y]]
  [(- size x 1) y])

(defn rotate-left
  [size [x y]]
  [y (- size x 1)])

(defn rotate-right
  [size [x y]]
  [(- size y 1) x])

(defn upside-down
  [size [x y]]
  [(- size x 1) (- size y 1)])

(def variation-list
  [do-nothing
   rotate-left
   rotate-left
   upside-down
   flip-horizontal
   (fn [size active-set]
     (flip-horizontal size (rotate-left size active-set)))
   (fn [size active-set]
     (flip-horizontal size (rotate-right size active-set)))
   (fn [size active-set]
     (flip-horizontal size (upside-down size active-set)))])

(defn transform
  [active-set size variation]
  (->> active-set
       (map (partial variation size))
       (set)))

(defn gen-variations*
  [size active-set]
  (map
    (partial transform active-set size)
    variation-list))

(def gen-variations (memoize gen-variations*))

(defn top-edge
  [active-set]
  (->> active-set
       (filter (fn [[_x y]]
                 (= y 0)))
       (map (fn [[x _y]]
              (Math/pow 2 x)))
       (apply +)))

(defn bottom-edge
  [active-set]
  (->> active-set
       (filter (fn [[_x y]]
                 (= y 9)))
       (map (fn [[x _y]]
              (Math/pow 2 x)))
       (apply +)))

(defn left-edge
  [active-set]
  (->> active-set
       (filter (fn [[x _y]]
                 (= x 0)))
       (map (fn [[_x y]]
              (Math/pow 2 y)))
       (apply +)))

(defn right-edge
  [active-set]
  (->> active-set
       (filter (fn [[x _y]]
                 (= x 9)))
       (map (fn [[_x y]]
              (Math/pow 2 y)))
       (apply +)))

(defn can-connect
  [active-set-1 active-set-2]
  (cond
    (= (bottom-edge active-set-1) (top-edge active-set-2))
    :down

    (= (top-edge active-set-1) (bottom-edge active-set-2))
    :up

    (= (left-edge active-set-1) (right-edge active-set-2))
    :left

    (= (right-edge active-set-1) (left-edge active-set-2))
    :right))

(defn load-input
  [input]
  (reduce
    (fn [a [k v]]
      (let [active-set (reduce
                         (fn [acc y]
                           (set/union
                             acc
                             (reduce
                               (fn [acc2 x]
                                 (if (get (get v y) x)
                                   (conj acc2 [x y])
                                   acc2))
                               #{}
                               (range (count (first v))))))
                         #{}
                         (range (count v)))]
        (assoc a k active-set)))
    {}
    input))

(defn part1
  [input]
  (->> (combo/combinations input 2)
       (mapcat (fn [[[k1 v1] [k2 v2]]]
                 (some
                   (fn [[v1-var v2-var]]
                     (when (can-connect v1-var v2-var)
                       [k1 k2]))
                   (combo/cartesian-product (gen-variations 10 v1) (gen-variations 10 v2)))))
       (remove nil?)
       (frequencies)
       (keep (fn [[k v]]
               (when (= 2 v)
                 k)))
       (apply *)))

(defn get-all-possible-grids
  [input]
  (->> (combo/combinations input 2)
       (mapcat (fn [[[_k1 v1] [_k2 v2]]]
                 (combo/cartesian-product (gen-variations 10 v1) (gen-variations 10 v2))))
       (reduce (fn [acc [v1-var v2-var]]
                 (let [direction (can-connect v1-var v2-var)]
                   (case direction
                     :left
                     (-> acc
                         (assoc-in [v1-var :left] v2-var)
                         (assoc-in [v2-var :right] v1-var))
                     :right
                     (-> acc
                         (assoc-in [v1-var :right] v2-var)
                         (assoc-in [v2-var :left] v1-var))
                     :up
                     (-> acc
                         (assoc-in [v1-var :up] v2-var)
                         (assoc-in [v2-var :down] v1-var))
                     :down
                     (-> acc
                         (assoc-in [v1-var :down] v2-var)
                         (assoc-in [v2-var :up] v1-var))
                     acc)))
               {})))

(defn collapse-grids
  [possible-grids]
  (let [start-grids (keep
                      (fn [[grid {:keys [left up]}]]
                        (when (and (nil? left)
                                   (nil? up))
                          grid))
                      possible-grids)]
    (some (fn [start-grid]
            (loop [big-grid #{}
                   active-grid start-grid
                   row-start-grid start-grid
                   [major-x major-y] [0 0]
                   [minor-x minor-y] [1 1]]
              (let [new-big-grid (if (contains? active-grid [minor-x minor-y])
                                   (conj big-grid [(- (+ minor-x (* major-x 8)) 1)
                                                   (- (+ minor-y (* major-y 8)) 1)])
                                   big-grid)
                    right-grid (get-in possible-grids [active-grid :right])
                    down-grid (get-in possible-grids [row-start-grid :down])]
                (cond
                  (and (= [8 8] [minor-x minor-y])
                       (= [11 11] [major-x major-y])
                       (nil? right-grid)
                       (nil? down-grid))
                  new-big-grid

                  (and (= [8 8] [minor-x minor-y])
                       (nil? right-grid)
                       (nil? down-grid))
                  nil

                  (and (= [8 8] [minor-x minor-y])
                       (nil? right-grid))
                  (recur new-big-grid
                         down-grid
                         down-grid
                         [0 (inc major-y)]
                         [1 1])

                  (= [8 8] [minor-x minor-y])
                  (recur new-big-grid
                         right-grid
                         row-start-grid
                         [(inc major-x) major-y]
                         [1 1])

                  (= 8 minor-x)
                  (recur new-big-grid
                         active-grid
                         row-start-grid
                         [major-x major-y]
                         [1 (inc minor-y)])

                  :else
                  (recur new-big-grid
                         active-grid
                         row-start-grid
                         [major-x major-y]
                         [(inc minor-x) minor-y])))))
          start-grids)))


(def relative-sea-monster-coordinates
  #{[0 1] [1 2] [4 2] [5 1] [6 1] [7 2] [10 2] [11 1] [12 1] [13 2] [16 2] [17 1] [18 0] [18 1] [19 1]})

(defn sea-monster-at-coordinates?
  [grid [origin-x origin-y]]
  (let [absolute-sea-monster-coordinates (->> relative-sea-monster-coordinates
                                              (map (fn [[x y]]
                                                     [(+ x origin-x) (+ y origin-y)]))
                                              (set))]
    (set/subset? absolute-sea-monster-coordinates grid)))

(defn print-grid
  [grid]
  (doseq [y (range 96)]
    (doseq [x (range 96)]
      (print (if (contains? grid [x y]) "#" ".")))
    (print "\n")))

(defn sea-monsters-in-grid
  [grid]
  (->> grid
       (gen-variations 96)
       (some (fn [variation]
               (print-grid variation)
               (let [monsters (->> (combo/cartesian-product (range 96) (range 96))
                                   (filter
                                     (fn [[y x]]
                                       (sea-monster-at-coordinates? variation [x y])))
                                   (count))]
                 (when (pos? monsters)
                   monsters))))))

(defn part2
  [input]
  (let [large-grid (->> input
                        (get-all-possible-grids)
                        (collapse-grids))
        num-monsters (sea-monsters-in-grid large-grid)]
    (- (count large-grid) (* num-monsters (count relative-sea-monster-coordinates)))))

(comment
  (load-input (utils/load-edn-input "day20.edn")))

(comment
  (part1 (load-input (utils/load-edn-input "day20.edn"))))

(comment
  (get-all-possible-grids (load-input (utils/load-edn-input "day20.edn"))))

(comment
  (part2 (load-input (utils/load-edn-input "day20.edn"))))
