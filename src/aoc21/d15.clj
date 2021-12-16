(ns aoc21.d15
  (:require [aoc21.core :as aoc :refer :all]))

(defn get-added-risk
  [ll]
  (let [rows (count ll)
        cols (-> ll first count)]
    (loop [[nll cur] [(-> (vec (repeat rows (vec (repeat cols Integer/MAX_VALUE))))
                          (assoc-in [0 0] 0))
                      [[0 0]]]]
      (if (empty? cur)
        nll
        (recur (reduce (fn [[nll nxt :as acc] p]
                         (let [pv (get-in nll p)]
                           (reduce (fn [[nll nxt :as acc] np]
                                     (let [npv (+ pv (get-in ll np))]
                                       (if (< npv (get-in nll np))
                                         [(assoc-in nll np npv)
                                          (conj nxt np)]
                                         acc)))
                                   acc (get-near-pts rows cols p))))
                       [nll []] cur))))))

(defn multiply-board
  [ll n]
  (let [rows (count ll)
        cols (-> ll first count)]
    (reduce (fn [nll irow]
              (reduce (fn [nll row]
                        (reduce (fn [nll icol]
                                  (reduce (fn [nll col]
                                            (assoc-in nll
                                                      [(+ (* irow rows) row)
                                                       (+ (* icol cols) col)]
                                                      (-> (get-in ll [row col])
                                                          (+ icol irow -1)
                                                          (mod 9) inc)))
                                          nll (range cols)))
                                nll (range n)))
                      nll (range rows)))
            (vec (repeat (* n rows) (vec (repeat (* n cols) 0))))
            (range n))))

(defn q1 [ll]
  (-> (get-added-risk ll) last last))

(defn q2 [ll]
  (-> (multiply-board ll 5)
      get-added-risk last last))

(def in (mapv #(mapv atoi %) (get-line-word "\\S")))

#_(q1 in)
#_(q2 in)
