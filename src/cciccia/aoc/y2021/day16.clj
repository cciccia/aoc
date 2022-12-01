(ns cciccia.aoc.y2021.day16
  (:require [clojure.string :as str]))

(defn hex-str->binary-seq
  [hex-str]
  (->> hex-str
       (mapcat (fn [c]
                 (let [cs (Integer/toBinaryString (Integer/parseInt (str c) 16))]
                   (str (str/join (repeat (- 4 (count cs)) 0)) cs))))))

(defn binary-str->dec
  [binary-str-list]
  (Integer/parseInt (str/join binary-str-list) 2))

(defn parse-packets
  [binary-seq target length?]
  (loop [binary-seq binary-seq
         state      :v
         target     target
         op         nil
         value      0
         output     {:versions 0 :length 0 :values []}]
    (if (or (zero? target)
            (< (count binary-seq) 4))
      output
      (case state
        :v
        (let [version (binary-str->dec (take 3 binary-seq))]
          (recur (drop 3 binary-seq)
                 :t
                 (if length?
                   (- target 3)
                   target)
                 op
                 value
                 (-> output
                     (update :versions + version)
                     (update :length + 3))))

        :t
        (let [o (binary-str->dec (take 3 binary-seq))]
          (recur (drop 3 binary-seq)
                 (if (= o 4) :a :o)
                 (if length?
                   (- target 3)
                   target)
                 (case o
                   0 +
                   1 *
                   2 min
                   3 max
                   4 nil
                   5 #(if (> %1 %2) 1 0)
                   6 #(if (< %1 %2) 1 0)
                   7 #(if (= %1 %2) 1 0))
                 value
                 (-> output
                     (update :length + 3))))

        :a
        (let [more? (= 1 (binary-str->dec (take 1 binary-seq)))
              v (+ (* 16 value) (binary-str->dec (take 4 (drop 1 binary-seq))))]
          (recur (drop 5 binary-seq)
                 (if more? :a :v)
                 (if length?
                   (- target 5)
                   (if more?
                     target
                     (dec target)))
                 op
                 (if more?
                   v
                   0)
                 (if more?
                   (-> output
                       (update :length + 5))
                   (-> output
                       (update :length + 5)
                       (update :values conj v)))))

        :o
        (if (zero? (binary-str->dec (take 1 binary-seq)))
          (let [subpackets (parse-packets (drop 16 binary-seq)
                                          (binary-str->dec (take 15 (drop 1 binary-seq)))
                                          true)
                to-drop    (+ 16 (binary-str->dec (take 15 (drop 1 binary-seq))))]
            (recur (drop to-drop binary-seq)
                   :v
                   (if length?
                     (- target to-drop)
                     (dec target))
                   op
                   value
                   {:length   (+ (:length output) (:length subpackets) 16)
                    :versions (+ (:versions output) (:versions subpackets))
                    :values   (conj (:values output) (apply op (:values subpackets)))}))
          (let [subpackets (parse-packets (drop 12 binary-seq)
                                          (binary-str->dec (take 11 (drop 1 binary-seq)))
                                          false)
                to-drop    (+ 12 (:length subpackets))]
            (recur (drop to-drop binary-seq)
                   :v
                   (if length?
                     (- target to-drop)
                     (dec target))
                   op
                   value
                   {:length   (+ (:length output) (:length subpackets) 12)
                    :versions (+ (:versions output) (:versions subpackets))
                    :values   (conj (:values output) (apply op (:values subpackets)))})))))))
