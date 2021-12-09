(ns aoc21.core
  (:require [clojure.string :as s]))

(defn get-res []
  (->> (s/split (str *ns*) #"\.")
       second
       (#(str "resources/" % ".txt"))
       slurp drop-last (apply str)))

(defn get-split
  ([]
   (get-split (get-res)))
  ([s]
   (get-split s " |,|\\n"))
  ([s delim]
   (s/split s (re-pattern delim))))

(defn get-line-split
  ([delim]
   (get-line-split (get-res) delim))
  ([s delim]
   (->> (s/split s #"\n")
        (mapv (fn [l]
                (s/split l (re-pattern delim)))))))

(defn get-line-word
  ([word]
   (get-line-word (get-res) word))
  ([s word]
   (->> (s/split s #"\n")
        (mapv (fn [l]
                (re-seq (re-pattern word) l))))))

(defn atoi
  [s]
  (Integer. (str s)))

(defn bint [s]
  (if s 1 0))

(defn get-num
  ([]
   (get-num (get-res)))
  ([s]
   (get-num s false))
  ([s minus?]
   (->> s
        (re-seq (if minus? #"[+-]?\d+" #"\d+"))
        (mapv atoi))))

(defn get-line-num
  ([]
   (get-line-num (get-res)))
  ([s]
   (->> (s/split s #"\n")
        (mapv (fn [l]
                (->> (re-seq #"[+-]?\d+" l)
                     (mapv atoi)))))))

(defn get-near-pts
  [rows cols p & [diagonal?]]
  (->> (repeat p)
       (mapv (fn [[ar ac] [br bc]]
               [(+ ar br) (+ ac bc)])
             (cond-> [[-1 0] [0 -1] [0 1] [1 0]]
               diagonal? (conj [-1 -1] [1 1] [-1 1] [1 -1])))
       (filter (fn [[r c]]
                 (and (>= r 0) (< r rows)
                      (>= c 0) (< c cols))))))

