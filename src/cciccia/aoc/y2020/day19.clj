(ns cciccia.aoc.y2020.day19
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.set :as set]))

(defn compile-rules
  [rules-file front-special back-special]
  (with-open [rdr (io/reader (io/resource rules-file))]
    (->> (line-seq rdr)
         (reduce (fn [acc line]
                   (if-let [[_ id-str match] (re-matches #"(\d+): \"(\w)\"" line)]
                     (let [id (read-string id-str)]
                       (assoc acc id (fn [[rules test front-bank string-set]]
                                       [rules
                                        test
                                        front-bank
                                        (->> string-set
                                             (map (fn [string]
                                                    (str string match)))
                                             (filter #(str/starts-with? test %))
                                             (set))])))
                     (let [[_ id-str stuff] (re-matches #"(\d+): (.*)" line)
                           id (read-string id-str)
                           ors (->> (str/split stuff #" \| ")
                                    (map (fn [or-group]
                                           (->> (str/split or-group #" ")
                                                (map (fn [id-str]
                                                       (let [id (read-string id-str)]
                                                         (fn [[rules test front-bank string-set]]
                                                           ((get rules id) [rules test front-bank string-set])))))
                                                (reverse)
                                                (apply comp)))))]
                       (assoc acc id (fn [[rules test front-bank string-set]]
                                       (let [new-string-set (->> (mapcat #(last (% [rules test front-bank string-set])) ors)
                                                                 (set))]
                                         (cond
                                           (and (= front-special id)
                                                (seq new-string-set)
                                                (pos? front-bank))
                                           [rules test front-bank (set/union new-string-set
                                                                             (last ((get rules front-special) [rules test (inc front-bank) new-string-set]))
                                                                             (last ((get rules back-special) [rules test (dec front-bank) new-string-set])))]

                                           (and (= front-special id)
                                                (seq new-string-set))
                                           [rules test front-bank (set/union new-string-set (last ((get rules front-special) [rules test (inc front-bank) new-string-set])))]

                                           (and (= back-special id)
                                                (seq new-string-set)
                                                (pos? front-bank))

                                           [rules test front-bank (set/union new-string-set (last ((get rules back-special) [rules test (dec front-bank) new-string-set])))]
                                           
                                           :else
                                           [rules test front-bank new-string-set])))))))
                 {}))))

(defn part1
  [rules tests-file]
  (with-open [rdr (io/reader (io/resource tests-file))]
    (->> (line-seq rdr)
         (filter (fn [line]
                   (contains? (last ((get rules 0) [rules line 0 #{""}])) line)))
         count)))

(comment
  (part1 (compile-rules "day19/rules.txt" nil nil) "day19/tests.txt"))

(comment
  (part1 (compile-rules "day19/rules.txt" 42 31) "day19/tests.txt"))


(comment
  (->> (combo/cartesian-product #{""} #{"a" "b"})
       (map #(apply str %))
       (set)))







