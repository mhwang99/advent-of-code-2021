(ns aoc21.d2
  (:require [clojure.string :as s]
            [aoc21.core :as aoc :refer :all]))

(defn q1 [l]
  (let [[x y] (reduce (fn [[x y] [d n]]
                        (case d
                          :f [(+ x n) y]
                          :u [x (- y n)]
                          :d [x (+ y n)]))
                      [0 0] l)]
    (* x y)))

(defn q2 [l]
  (let [[x y] (reduce (fn [[x y aim] [d n]]
                        (case d
                          :f [(+ x n) (+ y (* aim n)) aim]
                          :u [x y (- aim n)]
                          :d [x y (+ aim n)]))
                      [0 0 0] l)]
    (* x y)))

(def in
  (map (fn [[d n]]
         [(keyword (str (first d))) (atoi n)])
       (partition 2 (get-split))))

(def in2 [[:f 5] [:d 5] [:f 8] [:u 3] [:d 8] [:f 2]])

#_(q1 in)
#_(q2 in)
