(ns aoc21.d11
  (:require [aoc21.core :refer :all]))

(defn update-increase [ll pts]
  (reduce #(update-in %1 %2 inc) ll pts))

(defn update-flash [ll pts]
  (reduce (fn [[ll cnt] pt]
            (if (> (get-in ll pt) 9)
              (let [near-pts (get-near-pts 10 10 true pt)]
                [(-> (assoc-in ll pt -100)
                     (update-increase near-pts))
                 (inc cnt)])
              [ll cnt]))
          [ll 0] pts))

(defn update-zero [ll pts]
  (reduce #(if (pos? (get-in %1 %2)) %1
             (assoc-in %1 %2 0))
          ll pts))

(defn do-step [ll pts]
  (loop [ll (update-increase ll pts)
         cnt 0]
    (let [[ll curr-cnt] (update-flash ll pts)]
      (if (zero? curr-cnt)
        [ll cnt]
        (recur ll (+ cnt curr-cnt))))))

(defn q1
  [ll pts]
  (second
    (reduce (fn [[ll cnt] _]
              (let [[ll curr-cnt] (do-step ll pts)]
                [(update-zero ll pts) (+ curr-cnt cnt)]))
            [ll 0] (range 100))))

(defn q2
  [ll pts]
  (reduce (fn [ll i]
            (let [[ll curr-cnt] (do-step ll pts)]
              (if (= curr-cnt 100)
                (reduced (inc i))
                (update-zero ll pts))))
          ll (range)))

(def in (mapv #(mapv atoi %) (get-line-word "\\S")))
(def in-pts (for [r (range 10)
                  c (range 10)]
              [r c]))

#_(q1 in in-pts)
#_(q2 in in-pts)

