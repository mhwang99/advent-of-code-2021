(ns aoc21.d17
  (:require [aoc21.core :as aoc :refer :all]))

(defn abs [i] (if (< i 0) (- 0 i) i))
(defn max-reach [v] (/ (* v (inc v)) 2))

(defn q1 [[_ _ min-y _]]
  (-> min-y abs dec max-reach))

(defn q2 [[min-x max-x min-y max-y]]
  (let [svx (loop [i 1] (if (>= (max-reach i) min-x) i (recur (inc i))))
        evx max-x
        svy min-y
        evy (-> min-y abs dec)]
    (reduce (fn [cnt vx]
              (reduce (fn [cnt vy]
                        (+ cnt
                           (loop [[x y vx vy] [0 0 vx vy]]
                             (cond
                               (and (<= min-x x max-x)
                                    (<= min-y y max-y)) 1
                               (or (> x max-x) (< y min-y)) 0
                               :else (recur [(+ x vx) (+ y vy)
                                             (if (= vx 0)  0 (dec vx))
                                             (dec vy)])))))
                      cnt (range svy (inc evy))))
            0 (range svx (inc evx)))))

(def in (get-num (get-res) true))

#_(q1 in)
#_(q2 in)
