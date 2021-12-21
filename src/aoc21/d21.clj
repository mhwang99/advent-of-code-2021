(ns aoc21.d21
  (:require [aoc21.core :as aoc :refer :all]))

(defn q1 [sm]
  (loop [game {0 {:total 0 :start (get sm 1)}
               1 {:total 0 :start (get sm 2)}}
         p 0
         rolled 1]
    (let [score (-> rolled dec (mod 100) inc (* 3) (+ 3)
                    (+ (get-in game [p :start]))
                    dec (mod 10) inc)
          total (+ score (get-in game [p :total]))]
      (if (>= total 1000)
        (* (get-in game [(mod (inc p) 2) :total])
           (+ rolled 2))
        (recur (assoc game p {:total total :start score})
               (mod (inc p) 2)
               (+ rolled 3))))))

(defn make-universe [universe]
  (reduce (fn [m [total start-cnt]]
            (reduce (fn [m [start cnt]]
                      (reduce (fn [m [score ncnt]]
                                (update-in m [(+ total score) score]
                                           (fnil + 0) (* cnt ncnt)))
                              m (frequencies
                                  (for [a (range 1 4)
                                        b (range 1 4)
                                        c (range 1 4)]
                                    (-> (+ a b c start) dec (mod 10) inc)))))
                    m start-cnt))
          {} universe))

(defn q2 [sm]
  (loop [game {0 {0 {(get sm 1) 1}}
               1 {0 {(get sm 2) 1}}}
         wins {0 0 1 0}
         p 0 op 1]
    (if (or (empty? (get game p))
            (empty? (get game op)))
      (apply max (vals wins))
      (let [univ (make-universe (get game p))
            win-scores (filter #(>= % 21) (keys univ))
            wins (if (seq win-scores)
                   (let [win-cnt (->> win-scores
                                      (mapcat #(vals (get univ %)))
                                      (apply +))
                         lose-cnt (->> (keys (get game op))
                                       (mapcat #(vals (get-in game [op %])))
                                       (apply +))]
                     (update wins p + (* win-cnt lose-cnt)))
                   wins)]
        (recur (assoc game p (apply dissoc univ win-scores))
               wins op p)))))

(def in (into {} (get-line-num)))

#_(q1 in)
#_(q2 in)
