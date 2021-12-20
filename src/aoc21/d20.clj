(ns aoc21.d20
  (:require [aoc21.core :as aoc :refer :all]))

(defn bits->deci [bits] (Long/parseLong (apply str bits) 2))

(defn get-bit [alg ll r c d]
  (let [s [(get-in ll [(dec r) (dec c)] d) (get-in ll [(dec r) c] d) (get-in ll [(dec r) (inc c)] d)
           (get-in ll [     r  (dec c)] d) (get-in ll [     r  c] d) (get-in ll [     r  (inc c)] d)
           (get-in ll [(inc r) (dec c)] d) (get-in ll [(inc r) c] d) (get-in ll [(inc r) (inc c)] d)]
        i (bits->deci s)]
    (get alg i)))

(defn enhance [n {:keys [alg ll]}]
  (loop [ll ll
         n n
         sr 0 er (count ll)
         sc 0 ec (count (first ll))
         d (if (= 1 (first alg)) [0 1] [0 0])]
    (if (= 0 n) ll
      (let [m (reduce
                (fn [nll r]
                  (reduce
                    (fn [nll c]
                      (assoc-in nll [r c] (get-bit alg ll r c (first d))))
                    nll
                    (range (dec sc) (inc ec))))
                {}
                (range (dec sr) (inc er)))
            rl (->> m keys set)
            cl (->> m (mapcat (fn [[_ v]] (keys v))) set)]
        (recur m (dec n)
               (reduce min rl) (inc (reduce max rl))
               (reduce min cl) (inc (reduce max cl))
               [(second d) (first d)])))))

(defn q0 [n src]
  (->> (enhance n src)
       (mapcat (fn [[_ v]] (vals v)))
       (reduce +)))
(def q1 (partial q0 2))
(def q2 (partial q0 50))

(def in
  (let [ll (->> (get-line-word "\\S")
                (mapv (fn [s] (mapv #(if (= "#" %) 1 0) s))) )]
    {:alg (first ll)
     :ll (vec (drop 2 ll))}))

#_(q1 in)
#_(q2 in)
