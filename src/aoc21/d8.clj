(ns aoc21.d8
  (:require [aoc21.core :refer :all]
            [clojure.set :as set]))

(defn calc [digits]
  (let [digits (set digits)
        c8 (some #(when (= 7 (count %)) %) digits)
        c7 (some #(when (= 3 (count %)) %) digits)
        c1 (some #(when (= 2 (count %)) %) digits)
        c4 (some #(when (= 4 (count %)) %) digits)
        c3 (some #(when (and (= 5 (count %))
                             (= 3 (count (set/difference % c1)))) %) digits)
        c6 (some #(when (and (= 6 (count %))
                             (= 5 (count (set/difference % c1)))) %) digits)
        c5 (some #(when (and (= 5 (count %))
                             (= 0 (count (set/difference % c6)))) %) digits)
        c0 (some #(when (and (= 6 (count %))
                             (= 7 (count (set/union % c5)))) %) digits)
        c9 (some #(when (and (= 6 (count %))
                             (= 6 (count (set/union % c3)))) %) digits)
        c2 (some #(when (and (= 5 (count %))
                             (= 7 (count (set/union % c4)))) %) digits)]
    {c0 0 c1 1 c2 2 c3 3 c4 4 c5 5 c6 6 c7 7 c8 8 c9 9}))

(defn q1
  [l]
  (->> (map second l)
       flatten
       (filter #(#{2 3 4 7} (count %)))
       count))

(defn q2
  [l]
  (->> l
       (map (fn [[digits value]]
              (let [code-map (calc digits)]
                (->> (map code-map value)
                     (reduce #(+ (* 10 %1) %2) 0)))))
       (reduce +)))

(def in
  (->> (get-line-split " ")
       (map (fn [l]
              [(map set (take 10 l))
               (map set (drop 11 l))]))))

#_(q1 in)
#_(q2 in)
