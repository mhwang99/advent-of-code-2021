(ns aoc21.d7
  (:require [clojure.string :as s]
            [aoc21.core :as aoc :refer :all]))

(defn diff [a b] (if (< a b) (- b a) (- a b)))

(defn q0
  [fn-fuel l]
  (let [l (sort l)]
    (reduce (fn [min-fuel i]
              (let [fuel (reduce #(-> (diff %2 i) fn-fuel (+ %1)) 0 l)]
                (if (or (nil? min-fuel) (<= fuel min-fuel))
                  fuel
                  (reduced min-fuel))))
            nil (range (first l) (inc (last l))))))

(def sigma
 (memoize
  (fn [n]
    (if (zero? n) 0
      (+ (sigma (dec n)) n)))))

(def q1 (partial q0 identity))
(def q2 (partial q0 sigma))

(def in (get-num))
(def in2 [16,1,2,0,4,2,7,1,2,14])

#_(q1 in2)
#_(q2 in2)
#_(q1 in)
#_(q2 in)
