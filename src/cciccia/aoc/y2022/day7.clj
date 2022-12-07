(ns cciccia.aoc.y2022.day7
  (:require [cciccia.aoc.utils :as utils]))

(defn- build-filesystem
  [input]
  (loop [tape input
         pwd (list "/")
         files {}]
    (let [[instr & left] tape
          [_ dir] (when instr (re-matches #"\$ cd (.+)" instr))
          ls-cmd (when instr (re-matches #"\$ ls" instr))
          dir-list (when instr (re-matches #"dir (\w+)" instr))
          [_ size _file] (when instr (re-matches #"(\d+) (.+)" instr))]
      (cond
        (nil? instr)
        files

        (some? dir)
        (case dir
          "/"
          (recur left (list "/") files)
          ".."
          (recur left (pop pwd) files)
          (recur left (conj pwd dir) files))

        (some? ls-cmd)
        (recur left pwd files)

        (some? dir-list)
        (recur left pwd files)

        (some? size)
        (recur left pwd (reduce
                          (fn [acc dir]
                            (update acc dir #(+ (or % 0) (Integer/parseInt size))))
                          files
                          (map #(take-last (inc %) pwd) (range (count pwd)))))
        :else
        (throw (Exception. "Nope"))))))


(defn part1
  [input]
  (let [files (build-filesystem input)]
    (->> files
         vals
         (filter #(<= % 100000))
         (apply +))))

(defn part2
  [input]
  (let [files (build-filesystem input)
        unused (- 70000000 (get files (list "/")))
        needed (- 30000000 unused)]
    (->> files
         vals
         (filter #(>= % needed))
         (sort)
         (first))))

(comment
  (part1 (utils/lined-spaced-input->str "2022/day7.txt"))
  (part2 (utils/lined-spaced-input->str "2022/day7.txt")))
