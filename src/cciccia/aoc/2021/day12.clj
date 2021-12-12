(ns cciccia.aoc.2021.day12
  (:require [cciccia.aoc.utils :as utils]))

(defn build-connections
  [input]
  (reduce
    (fn [p [n1 n2]]
      (-> p
          (update n1 #(conj (or % #{}) n2))
          (update n2 #(conj (or % #{}) n1))))
    {}
    input))

(defn can-visit-next
  [connections loc visited]
  (filter
    (fn [n]
      (and (not= loc "end")
           (or (not (contains? (set visited) n))
               (re-matches #"^[A-Z]+$" n))))
    (get connections loc)))

(defn crawl
  [connections loc visited]
  (let [all-to-visit-next (can-visit-next connections loc visited)]
    (vec (apply concat [visited]
                (for [to-visit-next all-to-visit-next]
                  (crawl connections to-visit-next (conj visited to-visit-next)))))))


(defn part1
  [connections]
  (->> (crawl connections "start" ["start"])
       (filter #(contains? (set %) "end"))
       set
       count))

(defn can-visit-next2
  [connections loc visited]
  (let [visited-times (frequencies visited)]
    (filter
      (fn [n]
        (and (not= loc "end")
             (not= n "start")
             (or (not (contains? (set visited) n))
                 (re-matches #"^[A-Z]+$" n)
                 (not (some (fn [[k v]]
                              (and (re-matches #"^[a-z]+$" k)
                                   (> v 1)))
                            visited-times)))))
      (get connections loc))))

(defn crawl2
  [connections]
  (loop [terminal-paths []
         queue          [["start" ["start"]]]]
    (if (seq queue)
      (let [[loc visited] (first queue)
            all-to-visit-next (can-visit-next2 connections loc visited)]
        (if (seq all-to-visit-next)
          (recur terminal-paths (apply conj (subvec queue 1) (map #(vector % (conj visited %)) all-to-visit-next)))
          (recur (conj terminal-paths visited) (subvec queue 1))))
      terminal-paths)))

(defn part2
  [connections]
  (->> (crawl2 connections)
       (filter #(contains? (set %) "end"))
       set
       count))

(comment
  (part1 (build-connections (utils/load-edn-input "2021/day12.edn")))
  (part2 (build-connections (utils/load-edn-input "2021/day12.edn"))))

