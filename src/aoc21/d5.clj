(ns aoc21.d5
  (:require [clojure.string :as s]
            [aoc21.core :as aoc :refer :all]))

(defn get-line-points
  [[x1 y1 x2 y2]]
  (let [move [(compare x2 x1) (compare y2 y1)]]
    (loop [pnt [x1 y1]
           l []]
      (if (= pnt [x2 y2])
        (conj l pnt)
        (recur (mapv + pnt move) (conj l pnt))))))

(defn q1 [l]
  (->> l
       (filter (fn [[x1 y1 x2 y2]] (or (= x1 x2) (= y1 y2))))
       (mapcat get-line-points)
       (reduce #(update-in %1 %2 (fnil inc 0)) {})
       vals (mapcat vals) (filter #(> % 1)) count))

(defn q2 [l]
  (->> l
       (mapcat get-line-points)
       (reduce #(update-in %1 %2 (fnil inc 0)) {})
       vals (mapcat vals) (filter #(> % 1)) count))

(def in (partition 4 (get-num)))
(def in2 (->> (get-num "0,9 -> 5,9
                       8,0 -> 0,8
                       9,4 -> 3,4
                       2,2 -> 2,1
                       7,0 -> 7,4
                       6,4 -> 2,0
                       0,9 -> 2,9
                       3,4 -> 1,4
                       0,0 -> 8,8
                       5,5 -> 8,2")
              (partition 4)))

#_(q1 in2)
#_(q1 in)
#_(q2 in2)
#_(q2 in)
