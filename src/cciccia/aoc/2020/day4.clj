(ns cciccia.aoc.2020.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [spec-tools.core :as st]))

(def required #{:byr :iyr :eyr :hgt :hcl :ecl :pid})

(defn parse-input
  []
  (let [input (slurp (io/resource "day4.txt"))
        unparsed-passports (str/split input #"\n\n")]
    (map
      (fn [passport]
        (reduce
          (fn [acc2 [_ k v]]
            (assoc acc2 (keyword k) v))
          {}
          (re-seq #"([A-Za-z0-9#]+):([A-Za-z0-9#]+)" passport)))
      unparsed-passports)))

(defn part1
  [parsed-passports]
  (->> parsed-passports
       (filter #(set/subset? required (set (keys %))))
       (count)))

(defn range?
  [min max test]
  (<= min test max))

(defn height?
  [input]
  (when-let [[_ num unit] (re-matches #"^(\d{2,3})(in|cm)$" input)]
    (let [inum (Long/parseLong num)]
      (case unit
        "in"
        (<= 59 inum 76)
        "cm"
        (<= 150 inum 193)
        false))))

(s/def ::byr (st/spec {:spec (partial range? 1920 2002)
                       :type :long}))
(s/def ::iyr (st/spec {:spec (partial range? 2010 2020)
                       :type :long}))
(s/def ::eyr (st/spec {:spec (partial range? 2020 2030)
                       :type :long}))
(s/def ::hgt (st/spec {:spec height?}))
(s/def ::hcl (st/spec {:spec #(re-matches #"^#[0-9a-f]{6}$" %)}))
(s/def ::ecl (st/spec {:spec #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}}))
(s/def ::pid (st/spec {:spec #(re-matches #"^\d{9}$" %)}))

(s/def ::passport (st/spec {:spec (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid])}))

(defn part2
  [parsed-passports]
  (->> parsed-passports
       (filter #(s/valid? ::passport (st/coerce ::passport % st/string-transformer)))
       (count)))

(comment
  (part1 (parse-input)))

(comment
  (part2 (parse-input)))
