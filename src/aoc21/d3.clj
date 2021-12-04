(ns aoc21.d3
  (:require [clojure.string :as s]
            [aoc21.d1 :as d1]
            [aoc21.core :as aoc :refer :all]))

(defn opposite [s] (mapv #(if (= % \0) \1 \0) s))

(defn most-common [l]
  (let [{cnt0 \0 cnt1 \1} (frequencies l)]
    (if (> cnt0 cnt1) \0 \1)))

(defn least-common [l]
  (let [c (most-common l)]
    (if (= c \0) \1 \0)))

(defn q1 [l]
  (let [gam (apply str
                   (mapv (fn [i]
                           (most-common (mapv #(nth % i) l)))
                         (-> l first count range)))
        eps (apply str (opposite gam))]
    (* (Long/valueOf gam 2) (Long/valueOf eps 2))))

(defn get-rate
  [l f]
  (loop [l l
         i 0]
    (if (= (count l) 1)
      (Long/valueOf (first l) 2)
      (let [c (f (mapv #(nth % i) l))]
        (recur (filterv #(= c (nth % i)) l)
               (inc i))))))

(defn q2 [l]
  (* (get-rate l most-common)
     (get-rate l least-common)))


(def in (get-split))
(def in2 ["00100" "11110" "10110" "10111" "10101" "01111" "00111" "11100" "10000" "11001" "00010" "01010"])
#_(q1 in2)
#_(q2 in2)
#_(q1 in)
#_(q2 in)
