(ns cciccia.aoc.y2022.day16
  (:require [cciccia.aoc.utils :as utils]
            [clojure.math.combinatorics :as comb]
            [clojure.set :as set]))

(defn build-stuff
  [input]
  (->> input
       (reduce
         (fn [acc [source flow targets]]
           (-> (reduce
                 (fn [acc target]
                   (update-in acc [:links target] #(conj (or % #{}) source)))
                 acc
                 targets)
               (update-in [:links source] #(apply conj (or % #{}) targets))
               (assoc-in [:flows source] flow)))
         {:links     {}
          :flows     {}
          :distances {}})))

(defn build-shortest-paths
  [grid]
  (reduce
    (fn [acc [a b]]
      (loop [loc       a
             visited   #{}
             distances {a 0}]
        (let [neighbors     (set/difference (get-in grid [:links loc]) visited)
              new-distances (reduce
                              (fn [acc2 neighbor]
                                (-> acc2
                                    (update neighbor #(if %
                                                        (min % (inc (get distances loc)))
                                                        (inc (get distances loc))))))

                              distances
                              neighbors)
              new-loc       (->> new-distances
                                 (filter (fn [[k _v]] (not (contains? visited k))))
                                 (sort-by second)
                                 ffirst)]
          (if (= new-loc b)
            (assoc-in acc [:distances a b] (get new-distances new-loc))
            (recur new-loc (conj visited loc) new-distances)))))
    grid
    (comb/permuted-combinations
      (->> grid
           :flows
           (keep (fn [[loc val]]
                   (when (or (not= val 0) (= loc "AA"))
                     loc))))
      2)))

(defn get-potential-next-pressures
  [grid end visited loc i]
  (->> (set/difference (->> (get-in grid [:distances loc]) keys set) visited)
       (map
         (fn [dst]
           (let [i-to-elapse (inc (get-in grid [:distances loc dst]))]
             [dst
              (* (get-in grid [:flows dst]) (inc (- end i i-to-elapse)))
              (+ i i-to-elapse)])))
       (filter (fn [[_loc pressure _i]]
                 (pos? pressure)))))

(defn part1
  [grid]
  (->> (loop [to-try (utils/queue [[(list "AA") 1]])
              pressures {}]
         (if-let [[locs i] (peek to-try)]
           (let [cur-pressure (get pressures locs 0)
                 new-pressures (get-potential-next-pressures grid 30 (set locs) (first locs) i)]
             (if (seq new-pressures)
               (recur (apply conj (pop to-try) (map (fn [[next-loc _next-pressure next-i]] [(conj locs next-loc) next-i]) new-pressures))
                      (apply assoc pressures (mapcat (fn [[next-loc next-pressure _next-i]] [(conj locs next-loc) (+ cur-pressure next-pressure)]) new-pressures)))
               (recur (pop to-try) pressures)))
           pressures))
       (vals)
       (apply max)))

(defn part2
  [grid]
  (->> (loop [to-try (utils/queue [[[(list "AA") 1] [(list "AA") 1]]])
              pressures {}]
         (if-let [[[my-locs my-i] [ele-locs ele-i]] (peek to-try)]
           (let [cur-pressure (get pressures [my-locs ele-locs] 0)
                 new-my-pressures (get-potential-next-pressures grid 26 (set/union (set my-locs) (set ele-locs)) (first my-locs) my-i)
                 new-ele-pressures (get-potential-next-pressures grid 26 (set/union (set my-locs) (set ele-locs)) (first ele-locs) ele-i)
                 all-scenarios-and-pressures (->> (comb/cartesian-product new-my-pressures new-ele-pressures)
                                                  (reduce
                                                    (fn [acc [[next-my-loc next-my-pressure next-my-i] [next-ele-loc next-ele-pressure next-ele-i]]]
                                                      (if (and (not= next-my-loc next-ele-loc)
                                                               (not (contains? acc [[next-ele-loc next-ele-pressure next-ele-i]
                                                                                    [next-my-loc next-my-pressure next-my-i]])))
                                                        (-> acc
                                                            (conj [[next-my-loc next-my-pressure next-my-i]
                                                                   [next-ele-loc next-ele-pressure next-ele-i]]))
                                                        acc))
                                                    #{}))]
             (cond
               (seq all-scenarios-and-pressures)
               (let [next-scenarios (map (fn [[[next-my-loc _next-my-pressure next-my-i] [next-ele-loc _next-ele-pressure next-ele-i]]]
                                           [[(conj my-locs next-my-loc) next-my-i]
                                            [(conj ele-locs next-ele-loc) next-ele-i]]) all-scenarios-and-pressures)
                     next-pressures (map (fn [[[next-my-loc next-my-pressure _next-my-i] [next-ele-loc next-ele-pressure _next-ele-i]]]
                                           [[(conj my-locs next-my-loc) next-my-pressure]
                                            [(conj ele-locs next-ele-loc) next-ele-pressure]]) all-scenarios-and-pressures)]
                 (recur (apply conj (pop to-try) next-scenarios)
                        (apply assoc pressures (mapcat (fn [[[next-my-locs next-my-pressure] [next-ele-locs next-ele-pressure]]]
                                                         [[next-my-locs next-ele-locs] (+ cur-pressure next-my-pressure next-ele-pressure)]) next-pressures))))
               (seq new-my-pressures)
               (let [next-scenarios (map (fn [[next-my-loc _next-my-pressure next-my-i]]
                                           [[(conj my-locs next-my-loc) next-my-i]
                                            [ele-locs ele-i]]) new-my-pressures)
                     next-pressures (map (fn [[next-my-loc next-my-pressure _next-my-i]]
                                           [[(conj my-locs next-my-loc) next-my-pressure]
                                            [ele-locs 0]]) new-my-pressures)]
                 (recur (apply conj (pop to-try) next-scenarios)
                        (apply assoc pressures (mapcat (fn [[[next-my-locs next-my-pressure] [next-ele-locs next-ele-pressure]]]
                                                         [[next-my-locs next-ele-locs] (+ cur-pressure next-my-pressure next-ele-pressure)]) next-pressures))))
               (seq new-ele-pressures)
               (let [next-scenarios (map (fn [[next-ele-loc _next-ele-pressure next-ele-i]]
                                           [[my-locs my-i]
                                            [(conj ele-locs next-ele-loc) next-ele-i]]) new-ele-pressures)
                     next-pressures (map (fn [[next-ele-loc next-ele-pressure _next-ele-i]]
                                           [[my-locs my-i]
                                            [(conj ele-locs next-ele-loc) next-ele-pressure]]) new-ele-pressures)]
                 (recur (apply conj (pop to-try) next-scenarios)
                        (apply assoc pressures (mapcat (fn [[[next-my-locs next-my-pressure] [next-ele-locs next-ele-pressure]]]
                                                         [[next-my-locs next-ele-locs] (+ cur-pressure next-my-pressure next-ele-pressure)]) next-pressures))))

               :else
               (recur (pop to-try) pressures)))
           pressures))
       vals
       (apply max)))



(comment
  (part1 (build-shortest-paths (build-stuff (utils/load-edn-input "2022/day16.edn"))))
  (part2 (build-shortest-paths (build-stuff (utils/load-edn-input "2022/day16.edn")))))
