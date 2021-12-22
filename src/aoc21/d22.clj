(ns aoc21.d22
  (:require [aoc21.core :as aoc :refer :all]))

(defn parse-input [res]
  (->> (get-line-word res "on|off|[+-]?\\d+")
       (mapv (fn [l]
               (mapv #(cond
                        (= % "off") false
                        (= % "on") true
                        :else (atoi %)) l)))))

(defn intersection-xyx
  [[ax1 ax2 ay1 ay2 az1 az2]
   [bx1 bx2 by1 by2 bz1 bz2]]
  (when (and (<= ax1 bx2) (<= bx1 ax2)
             (<= ay1 by2) (<= by1 ay2)
             (<= az1 bz2) (<= bz1 az2))
    [(max ax1 bx1) (min ax2 bx2)
     (max ay1 by1) (min ay2 by2)
     (max az1 bz1) (min az2 bz2)]))

(defn sum-size
  [ll]
  (->> ll
       (map (fn [[on? x1 x2 y1 y2 z1 z2]]
              (* (if on? 1 -1)
                 (inc (- x2 x1))
                 (inc (- y2 y1))
                 (inc (- z2 z1)))))
       (reduce +)))

(defn q2 [ll]
  (sum-size
    (reduce (fn [rs [n-on? & n-r :as n-onr]]
              (cond-> (mapcat (fn [[on? & r :as onr]]
                                (let [ir (intersection-xyx r n-r)]
                                  (cond
                                    (nil? ir) [onr]
                                    (not= ir r) [onr (concat [(not on?)] ir)])))
                              rs)
                n-on? (conj n-onr)))
            [(first ll)] (rest ll))))

(defn q1 [ll]
  (q2
    (filterv (fn [[_ x1 x2 y1 y2 z1 z2]]
               (and (<= -50 x1 x2 50)
                    (<= -50 y1 y2 50)
                    (<= -50 z1 z2 50)))
             ll)))

(def in (parse-input (get-res)))

#_(q1 in)
#_(q2 in)

