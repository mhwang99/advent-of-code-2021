(ns aoc21.d17
  (:require [aoc21.core :as aoc :refer :all]))

(defn abs [i] (if (< i 0) (- 0 i) i))
(defn max-reach [v] (/ (* v (inc v)) 2))

(defn hit? [vx vy [min-x max-x min-y max-y]]
  (loop [[x y vx vy] [0 0 vx vy]]
    (cond
      (and (<= min-x x max-x)
           (<= min-y y max-y)) :hit
      (or (> x max-x) (< y min-y)) nil
      :else (recur [(+ x vx) (+ y vy)
                    (if (= vx 0)  0 (dec vx))
                    (dec vy)]))))

(defn q1 [[min-x max-x min-y max-y :as src]]
  (let [svx (loop [i 1] (if (>= (max-reach i) min-x) i (recur (inc i))))
        evx max-x
        svy min-y
        evy (-> min-y abs dec)]
    (max-reach
      (some (fn [vy]
              (some (fn [vx]
                      (when (hit? vx vy src)
                        vy))
                    (range svx (inc evx))))
            (reverse (range svy (inc evy)))))))

(defn q2 [[min-x max-x min-y max-y :as src]]
  (let [svx (loop [i 1] (if (>= (max-reach i) min-x) i (recur (inc i))))
        evx max-x
        svy min-y
        evy (-> min-y abs dec)]
    (reduce (fn [cnt vy]
              (reduce (fn [cnt vx]
                        (if (hit? vx vy src)
                          (inc cnt)
                          cnt))
                      cnt (range svx (inc evx))))
            0 (range svy (inc evy)))))

(def in (get-num (get-res) true))

#_(q1 in)
#_(q2 in)
