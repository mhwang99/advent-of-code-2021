(ns aoc21.d8
  (:require [aoc21.core :refer :all]
            [clojure.set :as set]))

(defn get-code-fn
  [digits]
  (let [cnt= #(or (nil? %1) (= %1 (count %2)))
        dset (set digits)]
    (fn [curr-cnt & [diff-code diff-cnt]]
      (some #(when (and (cnt= curr-cnt %)
                        (cnt= diff-cnt (set/difference % diff-code)))
               %) dset))))

(defn calc [digits]
  (let [get-code (get-code-fn digits)
        c8 (get-code 7)
        c7 (get-code 3)
        c1 (get-code 2)
        c4 (get-code 4)
        c3 (get-code 5 c1 3)
        c6 (get-code 6 c1 5)
        c5 (get-code 5 c6 0)
        c2 (get-code 5 c4 3)
        c9 (get-code 6 c3 1)
        c0 (get-code 6 c5 2)]
    (zipmap [c0 c1 c2 c3 c4 c5 c6 c7 c8 c9] (range))))

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
