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
        :when (and (not= x y) (not= x z) (not= y z))]
    [x y z fx fy fz]))

(defn deploy-convert [[dx dy dz x y z fx fy fz] pt]
  [(+ (* fx (get pt x)) dx)
   (+ (* fy (get pt y)) dy)
   (+ (* fz (get pt z)) dz)])

(defn deploy-rule [rule pt]
  (deploy-convert (into [0 0 0] rule) pt))

(defn mix-xyz [pt]
  (mapv (fn [rule] [(deploy-rule rule pt) rule])
        mix-rules))

(defn get-all-distance [a b]
  (map (fn [a [b rule]]
         (into (mapv - a b) rule))
       (repeat a)
       (mix-xyz b)))

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
                            (let [scanners (get scm k2)
                                  scm (apply dissoc scm (keys scanners))]
                              (->> scanners
                                   (map (fn [[k v]] [k (deploy-convert convert v)]))
                                   (update scm k1 into)))]
                           acc) acc) acc))
                   [m scm])
           recur))))

(defn q1 [m]
  (-> m q0 first first second count))

(defn abs [i] (if (< i 0) (- 0 i) i))

(defn q2 [m]
    (let [scanners (-> m q0 second (get 0) vals)]
      (->> (for [a scanners
                 b scanners
                 :when (pos? (compare a b))]
             [a b])
           (map (fn [[a b]]
                  (apply + (map #(abs (- %1 %2)) a b))))
           (reduce max))))


(defn parse-input [res]
  (loop [src (get-line-num res)
         ret []]
    (if (seq src)
      (let [l (take-while seq src)]
        (recur (drop (inc (count l)) src)
               (conj ret [(ffirst l) (set (rest l))])))
      (into {} ret))))

(def in (parse-input (get-res)))

#_(q1 in)
#_(q2 in)


