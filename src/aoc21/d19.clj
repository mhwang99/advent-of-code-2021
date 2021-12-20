(ns aoc21.d19
  (:require [aoc21.core :as aoc :refer :all]
            [clojure.string :as s]))

(def mix-rules
  (for [x (range 0 3)
        y (range 0 3)
        z (range 0 3)
        fx [-1 1]
        fy [-1 1]
        fz [-1 1]
        :when (let [f (if (or (= y (inc x))
                              (= z (inc y))
                              (= x (inc z)))
                        1 -1)]
                (and (not= x y) (not= x z) (not= y z)
                     (= (* f fx fy fz) 1)))]
    [x y z fx fy fz]))

(defn deploy-convert [[dx dy dz x y z fx fy fz] pt]
  [(+ (* fx (get pt x)) dx)
   (+ (* fy (get pt y)) dy)
   (+ (* fz (get pt z)) dz)])

(defn deploy-rule [[x y z fx fy fz] pt]
  [(* fx (get pt x)) (* fy (get pt y)) (* fz (get pt z))])

(defn get-all-distance [a b]
  (map (fn [a rule]
         (into (mapv - a (deploy-rule rule b)) rule))
       (repeat a)
       mix-rules))

(defn get-overlap [scan-a scan-b]
  (->> (for [a scan-a b scan-b]
         [a b])
       (mapcat (fn [[a b]] (get-all-distance a b)))
       frequencies
       (some (fn [[k v]] (when (>= v 12) k)))))

(defn q0 [m]
  (loop [[m scm] [m (->> (keys m)
                         (map (fn [k] [k {k [0 0 0]}]))
                         (into {}))]]
    (if (= (count m) 1) [m scm]
      (->> (for [k1 (keys m)
                 k2 (keys m)
                 :when (< k1 k2)]
             [k1 k2])
           (reduce (fn [[m scm :as acc] [k1 k2]]
                     (if-let [v1 (get m k1)]
                       (if-let [v2 (get m k2)]
                         (if-let [convert (get-overlap v1 v2)]
                           [(->> v2
                                 (mapv #(deploy-convert convert %))
                                 (into v1)
                                 (assoc (dissoc m k2) k1))
                            (->> (get scm k2)
                                 (map (fn [[k v]] [k (deploy-convert convert v)]))
                                 (update (dissoc scm k2) k1 into))]
                           acc) acc) acc))
                   [m scm])
           recur))))

(defn q1 [m]
  (-> m q0 first (get 0) count))

(defn q2 [m]
    (let [abs (fn [i] (if (< i 0) (- 0 i) i))
          scanners (-> m q0 second (get 0) vals)]
      (->> (for [a scanners
                 b scanners
                 :when (pos? (compare a b))]
             [a b])
           (map (fn [[a b]]
                  (apply + (map #(abs (- %1 %2)) a b))))
           (reduce max))))

(defn parse-input [res]
  (->> (get-line-num res)
       (partition-by empty?)
       (take-nth 2)
       (mapv (fn [l] [(ffirst l) (set (rest l))]))
       (into {})))

(def in (parse-input (get-res)))

#_(q1 in)
#_(q2 in)
