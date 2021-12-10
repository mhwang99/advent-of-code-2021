(ns aoc21.d10
  (:require [aoc21.core :as aoc :refer :all]))

(defn q0
  [on-error on-finish ll]
  (let [match {")" "("
               "]" "["
               "}" "{"
               ">" "<"}]
    (map (fn [l]
           (loop [stack []
                  l l]
             (let [c (first l)]
               (cond
                 (nil? c) (on-finish stack)
                 (#{"(" "[" "{" "<"} c)     (recur (conj stack c) (rest l))
                 (= (last stack) (match c)) (recur (vec (drop-last stack)) (rest l))
                 :else (on-error c)))))
         ll)))

(defn q1
  [ll]
  (->> ll
       (q0 {")" 3
            "]" 57
            "}" 1197
            ">" 25137}
           (constantly 0))
       (reduce +)))

(let [point {"(" 1
             "[" 2
             "{" 3
             "<" 4}]
  (defn get-complete-point
    [stack]
    (reduce (fn [acc c]
              (+ (* acc 5) (point c)))
            0 (reverse stack))))

(defn q2
  [ll]
  (let [pts (->> ll
                 (q0 (constantly nil) get-complete-point)
                 (remove nil?))]
    (-> pts sort (nth (int (/ (count pts) 2))))))

(def in
 (get-line-word "\\S"))

#_(q1 in)
#_(q2 in)
