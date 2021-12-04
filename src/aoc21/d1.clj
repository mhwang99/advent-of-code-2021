(ns aoc21.d1
  (:require [clojure.string :as s]
            [clojure.pprint :refer :all]
            [aoc21.core :as aoc :refer :all]))


(defn q1 [l]
  (->> (map #(> %2 %1) l (rest l))
       (filter identity)
       count))

(defn q2 [l]
  (let [l (map #(+ %1 %2 %3) l (rest l) (drop 2 l))]
    (q1 l)))

(def in (get-num (get-res)))
#_(q1 in)
#_(q2 in)

