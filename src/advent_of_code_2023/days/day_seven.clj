(ns advent-of-code-2023.days.day-seven
  (:require [advent-of-code-2023.utils.input-parsers :as input]
            [clojure.string :as str]))

(def card-to-strength-map
  {\A 12 \K 11 \Q 10 \J 9 \T 8 \9 7 \8 6 \7 5 \6 4 \5 3 \4 2 \3 1 \2 0})

(def hand-strength-map
  {:five-of-a-kind 6 :four-of-a-kind 5 :full-house 4 :three-of-a-kind 3 :two-pairs 2 :one-pair 1 :high-card 0})

(defn- hand-to-hand-strength
  [hand]
  (let [cards-with-count (reduce (fn [acc card] (assoc acc card (inc (get acc card 0)))) {} hand)
        len              (count cards-with-count)]
    (cond
     (= len 1) :five-of-a-kind
     (= len 2) (if (<= 2 (get cards-with-count (key (first cards-with-count))) 3) :full-house :four-of-a-kind)
     (= len 3) (if (= 3 (apply max (vals cards-with-count))) :three-of-a-kind :two-pairs)
     (= len 4) :one-pair
     :else     :high-card)))

(defn- parse-line
  [line]
  (let [[hand bid & _] (str/split line #"\s+")
        hand           (seq hand)
        bid            (Long/parseLong bid)]
    {:hand hand :hand-setrength (hand-to-hand-strength hand) :bid bid}))

(defn- compare-hands
  [a b]
  (let [[a-card b-card & _] (->>
                             (map vector a b)
                             (drop-while #(apply = %))
                             (first))]
   (compare (get card-to-strength-map a-card) (get card-to-strength-map b-card))))

(defn- compare-hands-and-bids
  [a b]
  (let [a-strength ((:hand-setrength a) hand-strength-map)
        b-strength ((:hand-setrength b) hand-strength-map)
        c (compare a-strength b-strength)]
    (if (not= c 0)
      c
      (compare-hands (:hand a) (:hand b)))))

(defn part-one
  []
  (->>
   (input/compose-input-filename "day_seven")
   (input/read-file-into-vector)
   (map parse-line)
   (sort compare-hands-and-bids)
   (map-indexed (fn [i {bid :bid}] (* (inc i) bid)))
   (apply +)))

(defn part-two
  []
  nil)