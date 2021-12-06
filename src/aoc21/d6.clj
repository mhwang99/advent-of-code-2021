(ns aoc21.d6
  (:require [clojure.string :as s]
            [aoc21.core :as aoc :refer :all]))

(defn q0
  [target l]
  (loop [i 0
         m (frequencies l)]
    (if (= i target)
      (->> m vals (reduce +))
      (recur (inc i)
             (reduce (fn [m [n cnt]]
                       (if (= n 0)
                         (-> m
                             (update 6 (fnil + 0) cnt)
                             (update 8 (fnil + 0) cnt))
                         (update m (dec n) (fnil + 0) cnt)))
                     {} m)))))

(def q1 (partial q0 80))
(def q2 (partial q0 256))

(def in (get-num))
(def in2 [3,4,3,1,2])

#_(q1 in2)
#_(q1 in)
#_(q2 in2)
#_(q2 in)
