(ns aoc21.d9
  (:require [aoc21.core :refer :all]))

(defn low?
  [l rows cols r c]
  (let [v (get-in l [r c])]
    (->> (get-near-pts rows cols [r c])
         (map #(< v (get-in l %)))
         (every? true?))))

(defn q1 [l]
  (let [rows (count l)
        cols (count (first l))]
    (->> (for [r (range rows)
               c (range cols)
               :when (low? l rows cols r c)]
           (inc (get-in l [r c])))
         (reduce +))))

(defn q2 [l]
  (let [rows (count l)
        cols (count (first l))]
    (->> (for [r (range rows)
               c (range cols)]
           [r c])
         (reduce (fn [[bl l] pt]
                   (if (= 9 (get-in l pt))
                     [bl l]
                     (loop [curr #{pt}
                            all curr
                            l (assoc-in l pt 9)]
                       (let [nxt (->> curr
                                      (mapcat (partial get-near-pts rows cols))
                                      (filter #(< (get-in l %) 9)))]
                         (if (seq nxt)
                           (recur (set nxt)
                                  (into all nxt)
                                  (reduce #(assoc-in %1 %2 9) l nxt))
                           [(conj bl (count all)) l])))))
                 [[] l])
         first sort reverse (take 3) (reduce *))))

(def in (mapv #(mapv atoi %) (get-line-word "\\S")))

#_(q1 in)
#_(q2 in)
