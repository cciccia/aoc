(ns cciccia.aoc.y2022.day17
  (:require [clojure.set :as set]
            [cciccia.aoc.utils :as utils]))

(def pieces-list
  [#{[0 0] [1 0] [2 0] [3 0]}
   #{[0 1] [1 2] [1 1] [1 0] [2 1]}
   #{[0 0] [1 0] [2 0] [2 1] [2 2]}
   #{[0 0] [0 1] [0 2] [0 3]}
   #{[0 0] [1 0] [0 1] [1 1]}])

(def moves-list
  [:jet :fall])

(defn hit-side?
  [piece]
  (some (fn [[x _y]]
          (or (< x 0) (> x 6))) piece))
(def hit-side?* (memoize hit-side?))

(defn collides?
  [grid piece]
  (or (hit-side?* piece)
      (seq (set/intersection grid piece))))
(def collides?* (memoize collides?))

(defn shift
  [dir piece]
  (case dir
    "|" (set (map (fn [[x y]] [x (dec y)]) piece))
    "<" (set (map (fn [[x y]] [(dec x) y]) piece))
    ">" (set (map (fn [[x y]] [(inc x) y]) piece))))

(defn move-over-or-down
  [grid dir piece]
  (let [new-piece (shift dir piece)]
    (if (collides?* grid new-piece)
      piece
      new-piece)))

(defn move-to
  [[x y] piece]
  (map (fn [[px py]]
         [(+ x px) (+ y py)]) piece))

(defn slide-grid-down
  [grid mag]
  (set (map (fn [[x y]]
              [x (- y mag)])
            grid)))

(defn max-height-above-zero
  [grid]
  (apply max (map second grid)))

(defn max-height
  [grid]
  (let [ys (map second grid)]
    (- (apply max ys)
       (apply min ys))))

(defn move-one
  [[x y] dir]
  (case dir
    :down [x (dec y)]
    :right [(inc x) y]
    :up [x (inc y)]
    :left [(dec x) y]))

(defn turn
  [cur-dir turn-dir]
  (case [cur-dir turn-dir]
    [:down :right] :left
    [:down :left] :right
    [:right :right] :down
    [:right :left] :up
    [:up :right] :right
    [:up :left] :left
    [:left :right] :up
    [:left :left] :down))

(defn all-necessary-cells
  [grid]
  (loop [[x y] [0 4]
         nec #{}
         dir :down]
    (cond
      (and (= dir :up)
           (= y 4))
      (set/intersection grid nec)

      (not (collides?* grid #{(move-one [x y] (turn dir :right))}))
      (recur (move-one [x y] (turn dir :right))
             nec
             (turn dir :right))

      (not (collides?* grid #{(move-one [x y] dir)}))
      (recur (move-one [x y] dir)
             (conj nec (move-one [x y] (turn dir :right)))
             dir)

      (not (collides?* grid #{(move-one [x y] (turn dir :left))}))
      (recur (move-one [x y] (turn dir :left))
             (-> nec
                 (conj nec (move-one [x y] (turn dir :right)))
                 (conj nec (move-one [x y] dir)))
             (turn dir :left))

      :else
      (recur (move-one [x y] (turn (turn dir :right) :right))
             (-> nec
                 (conj nec (move-one [x y] (turn dir :right)))
                 (conj nec (move-one [x y] dir))
                 (conj nec (move-one [x y] (turn dir :left))))
             (turn (turn dir :right) :right)))))

(defn part1
  [input target-num]
  (let [jets-seq (cycle input)
        pieces-seq (cycle pieces-list)
        moves-seq (cycle moves-list)]
    (loop [grid #{[0 0] [1 0] [2 0] [3 0] [4 0] [5 0] [6 0]}
           cur-piece nil
           num-pieces 0
           pieces-seq pieces-seq
           moves-seq moves-seq
           jets-seq jets-seq]
      (let [[new-move & rest-moves-seq] moves-seq
            [new-jet & rest-jets-seq] jets-seq]
        (cond
          (nil? cur-piece)
          (if (= num-pieces target-num)
            (max-height-above-zero grid)
            (let [[new-piece & rest-pieces-seq] pieces-seq]
              (recur grid
                     (move-to [2 (+ (max-height-above-zero grid) 4)] new-piece)
                     (inc num-pieces)
                     rest-pieces-seq
                     moves-seq
                     jets-seq)))

          (= new-move :jet)
          (let [new-piece (move-over-or-down grid new-jet cur-piece)]
            (recur grid
                   new-piece
                   num-pieces
                   pieces-seq
                   rest-moves-seq
                   rest-jets-seq))

          (= new-move :fall)
          (let [new-piece (move-over-or-down grid "|" cur-piece)]
            (if (= cur-piece new-piece)
              (recur (set/union grid cur-piece)
                     nil
                     num-pieces
                     pieces-seq
                     rest-moves-seq
                     jets-seq)
              (recur grid
                     new-piece
                     num-pieces
                     pieces-seq
                     rest-moves-seq
                     jets-seq)))

          :else
          (throw (Exception. "TF is goiing on?")))))))

(defn do-work
  [input target grid piece-idx jet-idx]
  (let [jets-len (count input)
        pieces-len (count pieces-list)]
    (loop [grid (or grid #{[0 0] [1 0] [2 0] [3 0] [4 0] [5 0] [6 0]})
           piece-idx (or piece-idx 0)
           jet-idx (or jet-idx 0)
           fun-cache {[grid piece-idx jet-idx] [0 0]}
           dead-height 0
           cur-piece nil
           num-pieces 0
           move :jet]
      (cond
        (and (nil? cur-piece)
             (some? target)
             (= num-pieces target))
        [dead-height]

        (nil? cur-piece)
        (let [new-piece (get pieces-list piece-idx)
              new-grid (all-necessary-cells grid)
              cache-val (get fun-cache [new-grid piece-idx jet-idx])]
          (if (and (some? cache-val) (pos? num-pieces))
            [dead-height [new-grid piece-idx jet-idx] (get fun-cache [new-grid piece-idx jet-idx]) [num-pieces dead-height]]
            (recur new-grid
                   (mod (inc piece-idx) pieces-len)
                   jet-idx
                   (assoc fun-cache [new-grid piece-idx jet-idx] [num-pieces dead-height])
                   dead-height
                   (move-to [2 4] new-piece)
                   (inc num-pieces)
                   :jet)))

        (= move :jet)
        (let [new-jet (get input jet-idx)
              new-piece (move-over-or-down grid new-jet cur-piece)]
          (recur grid
                 piece-idx
                 (mod (inc jet-idx) jets-len)
                 fun-cache
                 dead-height
                 new-piece
                 num-pieces
                 :fall))

        (= move :fall)
        (let [new-piece (move-over-or-down grid "|" cur-piece)]
          (if (= cur-piece new-piece)
            (let [new-grid (-> grid
                               (set/union cur-piece))
                  new-max-height (max-height-above-zero new-grid)]
              (recur (slide-grid-down new-grid new-max-height)
                     piece-idx
                     jet-idx
                     fun-cache
                     (+ dead-height new-max-height)
                     nil
                     num-pieces
                     :jet))
            (recur grid
                   piece-idx
                   jet-idx
                   fun-cache
                   dead-height
                   new-piece
                   num-pieces
                   :jet)))))))

(defn part2
  [input target]
  (let [[height & stuff] (do-work input target nil nil nil)]
    (if-not (seq stuff)
      height
      (let [[[grid piece-idx jet-idx] [s-num s-height] [e-num e-height]] stuff
            mult (long (Math/floor (/ (- target s-num) (- e-num s-num))))
            diff (- e-height s-height)
            todo (- target (+ s-num (* mult (- e-num s-num))))]
        (+ s-height (* diff mult) (first (do-work input todo grid piece-idx jet-idx)))))))

(comment
  (part1 (utils/load-edn-input "2022/day17-sample.edn") 66)
  (part2 (utils/load-edn-input "2022/day17.edn") 1000000000000))
