(ns cciccia.aoc.y2020.day21
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-ingredients-and-allergens
  []
  (with-open [rdr (io/reader (io/resource "day21.txt"))]
    (let [stuff (->> (line-seq rdr)
                     (reduce
                       (fn [acc line]
                         (let [[ingredients allergens] (str/split line #"[\(\)]")
                               ingredients (str/split ingredients #" ")
                               allergens (when allergens
                                           (str/split allergens #", "))]
                           (-> acc
                               (update :ingredients #(apply conj % ingredients))
                               (update :allergens set/union (set allergens)))))
                       {:ingredients []
                        :allergens #{}}))]
      (update stuff :ingredients frequencies))))

(defn find-potential-relationships
  [{total-ingredients :ingredients total-allergens :allergens}]
  (let [guilty-until-proven-innocent (->> total-allergens
                                          (reduce (fn [acc allergen]
                                                    (assoc acc allergen (set (keys total-ingredients))))
                                                  {}))]
    (with-open [rdr (io/reader (io/resource "day21.txt"))]
      (->> (line-seq rdr)
           (reduce
             (fn [acc line]
               (let [[ingredients allergens] (str/split line #"[\(\)]")
                     ingredients (str/split ingredients #" ")
                     allergens (when allergens
                                 (str/split allergens #", "))]
                 (if (seq allergens)
                   (do
                     (reduce
                       (fn [acc2 allergen]
                         (update acc2 allergen set/intersection (set ingredients)))
                       acc
                       allergens))
                   acc)))
             guilty-until-proven-innocent)))))

(defn part1
  []
  (let [{total-ingredients :allergens :as stuff} (parse-ingredients-and-allergens)]
    (->> (find-potential-relationships stuff)
         vals
         (apply concat)
         set
         (set/difference (set (keys total-ingredients)))
         (map (fn [ingredient]
                (get total-ingredients ingredient)))
         (apply +))))

(defn part2
  "fuck code"
  []
  "mxkh,gkcqxs,bvh,sp,rgc,krjn,bpbdlmg,tdbcfb")

(comment
  (parse-ingredients-and-allergens))

(comment
  (part1))

