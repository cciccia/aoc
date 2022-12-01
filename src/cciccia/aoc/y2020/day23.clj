(ns cciccia.aoc.y2020.day23)

(defn get-destination*
  [size current picked-up]
  (let [picked-up (set picked-up)]
    (cond
      (not (contains? picked-up (inc (mod (- current 2) size))))
      (inc (mod (- current 2) size))

      (not (contains? picked-up (inc (mod (- current 3) size))))
      (inc (mod (- current 3) size))

      (not (contains? picked-up (inc (mod (- current 4) size))))
      (inc (mod (- current 4) size))

      :else
      (inc (mod (- current 5) size)))))

(def get-destination (memoize get-destination*))

(defn get-next
  [om current]
  (get om current))

(defn part1
  [input moves]
  (let [size (count input)
        om (->> input
                (map-indexed #(vector %1 %2))
                (reduce
                  (fn [acc [i elem]]
                    (assoc acc elem (get input (mod (inc i) (count input)))))
                  {}))]
    (loop [com om
           i 0
           current (first input)]
      (if (= i moves)
        (apply * (drop 1 (take 3 (iterate (partial get-next com) 1))))
        (let [[n1 n2 n3 n4] (->> (iterate (partial get-next com) current)
                                 (take 5)
                                 rest)
              picked-up [n1 n2 n3]
              destination (get-destination size current picked-up)
              destination-plus (get com destination)]
          (recur
            (-> com
                (assoc current n4)
                (assoc destination n1)
                (assoc n3 destination-plus))
            (inc i)
            n4))))))

(comment
  (part1 [3 8 9 1 2 5 4 6 7] 10))

(comment
  (part1 [1 9 3 4 6 7 2 5 8] 100))

(comment
  (part1 (into [1 9 3 4 6 7 2 5 8] (range 10 (inc 1000000))) 10000000))


