(ns cciccia.aoc.2020.day14
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn str->masks
  [s]
  (reduce
    (fn [{:keys [one-mask zero-mask]} [i c]]
      (case c
        \0
        {:one-mask  one-mask
         :zero-mask (+ zero-mask (long (Math/pow 2 i)))}

        \1
        {:one-mask  (+ one-mask (long (Math/pow 2 i)))
         :zero-mask zero-mask}

        {:one-mask  one-mask
         :zero-mask zero-mask}))
    {:one-mask 0 :zero-mask 0}
    (map-indexed #(vector %1 %2) (reverse s))))


(defn part1
  []
  (with-open [rdr (io/reader (io/resource "day14.txt"))]
    (let [mem (reduce (fn [{:keys [mem masks]} line]
                        (if-let [[_ s] (re-matches #"mask = ([01X]+)" line)]
                          (let [new-masks (str->masks s)]
                            {:mem   mem
                             :masks new-masks})
                          (let [[_ address-str v-str] (re-matches #"mem\[(\d+)\] = (\d+)" line)
                                address      (read-string address-str)
                                v            (read-string v-str)
                                masked-value (-> v
                                                 (bit-or (:one-mask masks))
                                                 (bit-and-not (:zero-mask masks)))]
                            {:mem   (assoc mem address masked-value)
                             :masks masks})))
                      {:mem {} :masks {:one-mask 0 :zero-mask 0}}
                      (line-seq rdr))]
      (apply + (vals (:mem mem))))))

(defn expand-addr
  [s]
  (if (re-find #"X" s)
    (concat (expand-addr (str/replace-first s #"X" "0")) (expand-addr (str/replace-first s #"X" "1")))
    [s]))

(defn zp
  [n c]
  (loop [s (str n)]
    (if (< (.length s) c)
      (recur (str "0" s))
      s)))

(defn apply-mask
  [addr-str mask]
  (let [addr-str (zp addr-str 36)]
    (str/join (map
                (fn [i]
                  (if (= \0 (get mask i))
                    (get addr-str i \0)
                    (get mask i)))
                (range 36)))))

(defn part2
  []
  (with-open [rdr (io/reader (io/resource "day14.txt"))]
    (let [mem (reduce (fn [{:keys [mem mask]} line]
                        (if-let [[_ s] (re-matches #"mask = ([01X]+)" line)]
                          {:mem  mem
                           :mask s}
                          (let [[_ address-str v-str] (re-matches #"mem\[(\d+)\] = (\d+)" line)
                                addr            (read-string address-str)
                                addr-bin-string (Long/toBinaryString addr)
                                v               (read-string v-str)]
                            {:mem  (reduce
                                     (fn [acc addr]
                                       (assoc acc (Long/parseLong addr 2) v))
                                     mem
                                     (expand-addr (apply-mask addr-bin-string mask)))
                             :mask mask})))
                      {:mem {} :mask []}
                      (line-seq rdr))]
      (apply + (vals (:mem mem))))))


(comment
  (part1))

(comment
  (part2))