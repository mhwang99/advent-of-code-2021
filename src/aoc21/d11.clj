(ns aoc21.d11
  (:require [aoc21.core :refer :all]))

(defn update-increase
  [ll pts]
  (reduce (fn [ll pt]
            (let [v (get-in ll pt)]
              (if (> v -1)
                (assoc-in ll pt (inc v))
                ll)))
          ll pts))

(defn update-flash
  [ll pts]
  (reduce (fn [[ll cnt] pt]
            (let [v (get-in ll pt)]
              (if (> v 9)
                (let [near-pts (get-near-pts 10 10 true pt)]
                  [(-> (assoc-in ll pt -1)
                       (update-increase near-pts))
                   (inc cnt)])
                [ll cnt])))
          [ll 0] pts))

(defn update-zero
  [ll pts]
  (reduce (fn [ll pt]
            (let [v (get-in ll pt)]
              (if (= v -1)
                (assoc-in ll pt 0)
                ll)))
          ll pts))

(defn get-step
  [ll pts]
  (loop [ll (update-increase ll pts)
         prev-cnt nil
         cnt 0]
    (if (and prev-cnt (= prev-cnt cnt))
      [ll cnt]
      (let [[ll curr-cnt] (update-flash ll pts)]
        (recur ll cnt (+ cnt curr-cnt))))))

(defn q1
  [ll pts]
  (second
    (reduce (fn [[ll cnt] i]
              (let [[ll curr-cnt] (get-step ll pts)]
                [(update-zero ll pts) (+ curr-cnt cnt)]))
            [ll 0] (range 100))))

(defn q2
  [ll pts]
  (reduce (fn [ll i]
            (let [[ll curr-cnt] (get-step ll pts)]
              (if (= curr-cnt 100)
                (reduced (inc i))
                (update-zero ll pts))))
          ll (range)))

(def in (mapv #(mapv atoi %) (get-line-word "\\S")))
(def in-pts (doall
              (for [r (range 10)
                    c (range 10)]
                [r c])))

#_(q1 in in-pts)
#_(q2 in in-pts)

