(ns aoc21.d4
  (:require [clojure.string :as s]
            [aoc21.core :as aoc :refer :all]))

(def in (get-line-num))
(def in2 (get-line-num
           "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1

           22 13 17 11  0
           8  2 23  4 24
           21  9 14 16  7
           6 10  3 18  5
           1 12 20 15 19

           3 15  0  2 22
           9 18 13 17  5
           19  8  7 25 23
           20 11 10 24  4
           14 21 16 12  6

           14 21 17 24  4
           10 16 15  9 19
           18  8 23 26 20
           22 11 13  6  5
           2  0 12  3  7"))

(defn check-board
  [board call]
  (mapv (fn [row]
          (mapv #(when (not= % call) %) row))
        board))


(defn bingo?
  [board]
  (or (some (fn [row] (every? nil? row)) board)
      (some (fn [i]
              (every? nil? (mapv #(nth % i) board)))
            (-> board first count range))))

(defn sum-all
  [board]
  (reduce (fn [ret row]
            (->> (filter identity row)
                 (reduce +)
                 (+ ret)))
          0 board))

(defn q1 [l]
  (let [calls (first l)
        boards (->> l rest (partition 6) (map rest))]
    (loop [calls calls
           boards boards]
      (when (seq calls)
        (let [call (first calls)
              boards (mapv #(check-board % call) boards)
              remain (some #(when (bingo? %)
                              (sum-all %)) boards)]
          (if remain
            (* call remain)
            (recur (rest calls) boards)))))))

(defn q2 [l]
  (let [calls (first l)
        boards (->> l rest (partition 6) (map rest))]
    (loop [calls calls
           boards boards]
      (when (seq calls)
        (let [call (first calls)
              boards (mapv #(check-board % call) boards)
              boards2 (remove bingo? boards)]
          (if (= (count boards2) 0)
            (* call (sum-all (first boards)))
            (recur (rest calls) boards2)))))))

#_(q1 in2)
#_(q2 in2)
#_(q1 in)
#_(q2 in)







