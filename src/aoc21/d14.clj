(ns aoc21.d14
  (:require [aoc21.core :refer :all]))

(defn parse [src]
  (let [[[template] _ & rules] (get-line-word src "[A-Z]+")
        rules (->> (mapv (fn [[[c1 c2] [v]]]
                           [[c1 c2] v])
                         rules)
                   (into {}))]
    {:template template
     :rules rules}))

(defn q0
  [step {:keys [template rules]}]
  (let [cnt-map (->> (range step)
                     (reduce (fn [cnt-map _]
                               (reduce (fn [nm [[c1 c2 :as k] v]]
                                         (let [nc (rules k)]
                                           (-> nm
                                               (update [c1 nc] (fnil (partial + v) 0))
                                               (update [nc c2] (fnil (partial + v) 0)))))
                                       {} cnt-map))
                             (frequencies (map vector template (rest template))))
                     (reduce (fn [cnt-map [[k] v]]
                               (update cnt-map k (fnil (partial + v) 0)))
                             {}))
        cnt-list (vals (update cnt-map (last template) (fnil inc inc)))]
    (- (reduce max cnt-list) (reduce min cnt-list))))

(def q1 (partial q0 10))

(def q2 (partial q0 40))

(def in (parse (get-res)))

#_(q1 in)
#_(q2 in)
