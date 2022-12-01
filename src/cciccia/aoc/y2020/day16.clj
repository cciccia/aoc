(ns cciccia.aoc.y2020.day16
  (:require [cciccia.aoc.utils :as utils]
            [clojure.set :as set]))


(defn mark-valid-inputs
  [input-vec-of-vecs]
  (reduce
    (fn [acc [v1 v2 v3 v4]]
      (as-> acc $
            (apply conj $ (range v1 (inc v2)))
            (apply conj $ (range v3 (inc v4)))))
    #{}
    input-vec-of-vecs))

(defn part1
  [tickets valid]
  (->> tickets
       (map (fn [ticket]
              (apply + (set/difference (set ticket) valid))))
       (apply +)))

(defn valid-tickets
  [tickets valid]
  (->> tickets
       (filter (fn [ticket]
                 (set/subset? (set ticket) valid)))
       vec))

(defn mark-specific-valid-inputs
  [input-vec-of-vecs]
  (reduce
    (fn [acc [i [v1 v2 v3 v4]]]
      (assoc acc i (set/union (set (range v1 (inc v2))) (set (range v3 (inc v4))))))
    {}
    (map-indexed #(vector %1 %2) input-vec-of-vecs)))

(defn pivot
  [valid-tickets]
  (apply mapv vector valid-tickets))

(defn part2
  [tickets-by-field-index specific-valid-fields my-ticket]
  (let [potential-valid-field-ids (->> tickets-by-field-index
                                       (map
                                         (fn [field-values]
                                           (set (keep (fn [[field-id valid-field-values]]
                                                        (when (set/subset? field-values valid-field-values)
                                                          field-id))
                                                      specific-valid-fields)))))
        potential-valid-field-ids-sorted (->> potential-valid-field-ids
                                              (map-indexed #(vector %1 %2))
                                              (sort-by #(count (second %))))
        valid-field-ids (loop [ids {}
                               left potential-valid-field-ids-sorted]
                          (if-let [[i s] (first left)]
                            (let [id (first (set/difference s (set (vals ids))))]
                              (recur (assoc ids i id) (rest left)))
                            ids))]
    valid-field-ids
    (->> my-ticket
         (map-indexed #(vector %1 %2))
         (keep (fn [[i field]]
                 (when (<= (get valid-field-ids i) 5)
                   field)))
         (apply *))))

(comment
  (part1 (utils/load-edn-input "day16/tickets.edn") (mark-valid-inputs (utils/load-edn-input "day16/constraints.edn"))))

(comment
  (part2 (pivot (utils/load-edn-input "day16/valid_tickets.edn")) (mark-specific-valid-inputs (utils/load-edn-input "day16/constraints.edn")) [127 109 139 113 67 137 71 97 53 103 163 167 131 83 157 101 107 79 73 89]))
