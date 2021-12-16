(ns aoc21.d16
  (:require [aoc21.core :as aoc :refer :all]))

(def hex
  {\0 "0000" \1 "0001" \2 "0010" \3 "0011"
   \4 "0100" \5 "0101" \6 "0110" \7 "0111"
   \8 "1000" \9 "1001" \A "1010" \B "1011"
   \C "1100" \D "1101" \E "1110" \F "1111"})

(defn hex->bits [s] (mapcat hex s))
(defn split [n bits] [(take n bits) (drop n bits)])
(defn bits->deci [bits] (Long/parseLong (apply str bits) 2))

(declare get-packet)

(defn get-packet-oper-len [bits]
  (let [[len bits] (split 15 bits)
        len (bits->deci len)
        sub (loop [bits bits
                   sub []
                   len len]
              (let [{clen :len :as packet} (get-packet bits)
                    sub (conj sub packet)
                    len (- len clen)]
                (if (<= len 0) sub
                  (recur (drop clen bits) sub len))))]
    [sub (+ len 15)]))

(defn get-packet-oper-cnt [bits]
  (let [[cnt bits] (split 11 bits)
        cnt (bits->deci cnt)
        [sub len] (loop [bits bits
                         sub []
                         len 11
                         cnt cnt]
                    (if (= cnt 0) [sub len]
                      (let [{clen :len :as packet} (get-packet bits)
                            sub (conj sub packet)
                            len (+ len clen)]
                        (recur (drop clen bits) sub len (dec cnt)))))]
    [sub len]))

(defn get-packet-literal [bits]
  (loop [bits bits
         len 0
         ret []]
    (let [ret (into ret (take 4 (rest bits)))]
      (if (= (first bits) \0)
        [(bits->deci ret) (+ len 5)]
        (recur (drop 5 bits) (+ len 5) ret)))))

(defn get-packet [bits]
  (let [[ver bits] (split 3 bits)
        [typ bits] (split 3 bits)
        [ver typ] (map bits->deci [ver typ])
        packet {:ver ver :typ typ}]
    (if (= typ 4)
      (let [[value len] (get-packet-literal bits)]
        (assoc packet
               :value value
               :len (+ len 6)))
      (let [[sub len] (if (= (first bits) \0)
                        (get-packet-oper-len (rest bits))
                        (get-packet-oper-cnt (rest bits)))]
        (assoc packet
               :sub sub
               :len (+ len 7))))))

(defn parse-packet
  [{:keys [typ value sub]}]
  (let [subv (map parse-packet sub)]
    (case typ
      0 (apply + subv)
      1 (apply * subv)
      2 (apply min subv)
      3 (apply max subv)
      4 value
      5 (if (apply > subv) 1 0)
      6 (if (apply < subv) 1 0)
      (if (apply = subv) 1 0))))

(defn q1 [s]
  (->> s hex->bits get-packet
       (reduce-walk (fn [ver node]
                      (if (map? node)
                        (+ ver (:ver node))
                        ver))
                    0)))

(defn q2 [s]
  (->> s hex->bits get-packet parse-packet))

(def in (get-res))

#_(q1 in)
#_(q2 in)

