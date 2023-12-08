(ns cciccia.aoc.y2023.day8
  (:require [cciccia.aoc.utils :as utils]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(defn build-graph
  [graph-lines]
  (->> graph-lines
       (map (fn [line]
              (let [[_ cur left right] (re-matches #"(\w{3}) = \((\w{3}), (\w{3})\)" line)]
                [cur {:left left :right right}])))
       (into {})))

(defn part1
  [directions-str graph-lines]
  (let [graph (build-graph graph-lines)]
    (reduce
      (fn [acc [i dir]]
        (if (= "ZZZ" acc)
          (reduced i)
          (get-in graph [acc (case dir \L :left \R :right)])))
      "AAA"
      (map-indexed #(vector %1 %2) (cycle directions-str)))))

(defn part2
  [directions-str graph-lines]
  (let [graph (build-graph graph-lines)]
    (reduce
      (fn [accs [i dir]]
        (let [new-accs (mapv
                         (fn [acc]
                           (get-in graph [acc (case dir \L :left \R :right)]))
                         accs)]
          (if (every? #(= \Z (last %)) new-accs)
            (reduced (inc i))
            new-accs)))
      (filter #(= \A (last %)) (keys graph))
      (map-indexed #(vector %1 %2) (cycle directions-str)))))

(defn build-path-seq
  [graph directions-vec cur]
  (loop [cur cur
         i 0
         di 0
         path []
         seen {}]
    (let [dir (get directions-vec di)
          new-cur (get-in graph [cur (case dir "L" :left "R" :right)])
          new-di (mod (inc di) (count directions-vec))]
      (if (= \Z (last cur))
        (if (contains? seen [cur di])
          [(- i (get seen [cur di])) path]
          (recur new-cur
                 (inc i)
                 new-di
                 (conj path [i cur di])
                 (assoc seen [cur di] i)))
        (recur new-cur
               (inc i)
               new-di
               path
               seen)))))

(defn lazy-zed
  [len-and-path i pos]
  (let [[len path] len-and-path]
    (lazy-seq
      (if (= pos (dec (count path)))
        (cons
          (+ i len (- (first (get path 0)) (first (get path pos))))
          (lazy-zed len-and-path (+ len i) 0))
        (cons
          (+ i (- (first (get path (inc pos))) (first (get path pos))))
          (lazy-zed len-and-path (+ i (- (first (get path (inc pos))) (first (get path pos)))) (inc pos)))))))


(defn part2-faster-but-lol-get-the-lcm
  [directions-str graph-lines]
  (let [graph (build-graph graph-lines)
        directions-vec (str/split directions-str #"")
        len-paths (->> (filter #(= \A (last %)) (keys graph))
                       (mapv #(build-path-seq graph directions-vec %)))]
    (println len-paths) ;you can stop here and take the LCM of the numbers you get in these because there is only ever one state encountered in each pather
    (loop [travels (mapv #(vector 0 (lazy-zed % 0 0)) len-paths)]
      (let [traveled (map first travels)]
        (if (and (apply = traveled) (pos? (first traveled)))
          (first traveled)
          (let [[itravel & others] travels
                [_i travel] itravel
                [new-travel & rest-travels] travel]
            (recur (sort-by first (conj others [new-travel rest-travels])))))))))

(comment
  (part1 (slurp (io/resource "2023/day8-directions.txt"))
         (utils/lined-spaced-input->str "2023/day8-graph.txt"))
  (part2-faster-but-lol-get-the-lcm (slurp (io/resource "2023/day8-sample-directions.txt"))
                                    (utils/lined-spaced-input->str "2023/day8-sample-graph.txt")))