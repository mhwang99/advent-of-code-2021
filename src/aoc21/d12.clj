(ns aoc21.d12
  (:require [aoc21.core :refer :all]))

(defn get-all-next
  [m curr fn-old]
  (mapcat
    (fn [{:keys [path old]}]
      (->> (last path)
           (get m)
           (keep (fn [p]
                   (let [[p op] (cond
                                    (= "start" p) nil
                                    (<= 65 (int (first p)) 90) [p]
                                    (old p) (fn-old old p)
                                    :else [p p])]
                     (when p
                       {:path (conj path p)
                        :old (if op (conj old op) old)}))))))
    curr))


(defn q0 [m fn-old]
  (loop [done []
         paths [{:path ["start"]
                 :old #{}}]]
    (if (empty? paths)
      (count done)
      (let [{paths false ended true} (->> (get-all-next m paths fn-old)
                                          (group-by #(-> % :path last (= "end"))))]
        (recur (if ended
                 (into done (map :path ended))
                 done)
               paths)))))

(defn q1 [m]
  (q0 m (constantly nil)))

(defn q2 [m]
  (q0 m #(when-not (%1 2) [%2 2])))

(def in
  (reduce (fn [m [s e]]
            (-> m
                (update s (fnil conj #{}) e)
                (update e (fnil conj #{}) s)))
          {} (get-line-word "[a-zA-Z]+")))

#_(q1 in)
#_(q2 in)
