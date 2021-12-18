(ns aoc21.d18
  (:require [aoc21.core :as aoc :refer :all]
            [clojure.string :as s]))

(defn splits?
  ([fish] (splits? fish []))
  ([fish ks]
   (when-let [v (get-in fish ks)]
     (if (number? v)
       (when (> v 9)
         [ks v])
       (or (splits? fish (conj ks 0))
           (splits? fish (conj ks 1)))))))

(defn explode?
  ([fish] (explode? fish []))
  ([fish ks]
   (let [v (get-in fish ks)]
     (when (vector? v)
       (let [[lv rv] v]
         (if (and (number? lv) (number? rv) (>= (count ks) 4))
           [ks lv rv]
           (or (explode? fish (conj ks 0))
               (explode? fish (conj ks 1)))))))))

(defn get-near [d fish ks]
  (loop [me (peek ks)
         ks (pop ks)]
    (cond
      (= me d) (loop [ks (conj ks (if (zero? d) 1 0))]
                 (when-let [v (get-in fish ks)]
                   (if (number? v) [ks v]
                     (recur (conj ks d)))))
      (seq ks) (recur (peek ks) (pop ks))
      :else nil)))

(def get-left (partial get-near 1))
(def get-right (partial get-near 0))

(defn splits [fish]
  (when-let [[ks v] (splits? fish)]
    (let [lv (quot v 2)
          rv (- v lv)]
      (assoc-in fish ks [lv rv]))))

(defn explode [fish]
  (when-let [[ks lv rv] (explode? fish)]
    (let [[lks llv] (get-left fish ks)
          [rks rrv] (get-right fish ks)
          llv (if lks (+ llv lv) 0)
          rrv (if rks (+ rrv rv) 0)]
      (cond-> (assoc-in fish ks 0)
        lks (assoc-in lks llv)
        rks (assoc-in rks rrv)))))

(defn magnitude
  ([v] (magnitude v 1))
  ([v t]
   (if (number? v)
     (* t v)
     (* t (+ (magnitude (first v) 3)
             (magnitude (second v) 2))))))

(defn reduce-fish [fish]
  (if-let [nfish (explode fish)]
    (recur nfish)
    (if-let [nfish (splits fish)]
      (recur nfish)
      fish)))

(defn q1 [fishes]
  (magnitude
    (reduce
      (fn [fish new-fish]
        (reduce-fish [fish new-fish]))
      fishes)))

(defn pairs [l]
  (->> (reduce (fn [l e]
                 (->> l
                      (filter #(< (count %) 2))
                      (map #(conj % e))
                      (into l)))
               [[]] l)
       (filter #(= (count %) 2))
       (mapcat (fn [[a b]] [[a b] [b a]]))))

(defn q2 [fishes]
  (->> (pairs fishes)
       (reduce (fn [mx fish]
                 (max mx (magnitude (reduce-fish fish))))
               0)))

(def in (mapv read-string (s/split (get-res) #"\n")))

#_(q1 in)
#_(q2 in)



