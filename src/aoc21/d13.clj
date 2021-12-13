(ns aoc21.d13
  (:require [aoc21.core :refer :all]))

(defn fold
  [pts [axl p]]
  (reduce (fn [acc [y x :as pt]]
            (let [pt (cond
                       (and (= axl :x) (> x p)) [y (- (* 2 p) x)]
                       (and (= axl :y) (> y p)) [(- (* 2 p) y) x]
                       :else pt)]
              (conj acc pt)))
          #{} pts))

(defn q1
  [[pts inst]]
  (count (fold pts (first inst))))

(defn q2
  [[pts inst]]
  (let [pts (reduce fold pts inst)
        rows (reduce #(max %1 (first %2)) 0 pts)
        cols (reduce #(max %1 (second %2)) 0 pts)
        board (vec (repeat (inc rows) (vec (repeat (inc cols) \.))))
        board (reduce #(assoc-in %1 %2 \#) board pts)]
    (doseq [row board]
      (println (apply str row)))))

(defn parse [src]
  (let [res (get-line-word src "x|y|\\d+")
        pts (->> (take-while some? res)
                 (mapv (fn [[x y]] [(atoi y) (atoi x)])))
        inst (->> (drop (inc(count pts)) res)
                  (mapv (fn [[axl p]]
                          [(keyword axl) (atoi p)])))]
    [(set pts) inst]))

(def in (parse (get-res)))

#_(q1 in)
#_(q2 in)

