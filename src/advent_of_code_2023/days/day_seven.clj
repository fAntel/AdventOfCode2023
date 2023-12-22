(ns advent-of-code-2023.days.day-seven
  (:require [advent-of-code-2023.utils.input-parsers :as input]
            [clojure.string :as str]))

(def card-to-strength-map
  {\A 12 \K 11 \Q 10 \J 9 \T 8 \9 7 \8 6 \7 5 \6 4 \5 3 \4 2 \3 1 \2 0})

(def joker-card-to-strength-map
  {\A 12 \K 11 \Q 10 \T 9 \9 8 \8 7 \7 6 \6 5 \5 4 \4 3 \3 2 \2 1 \J 0})

(def hand-strength-map
  {:five-of-a-kind 6 :four-of-a-kind 5 :full-house 4 :three-of-a-kind 3 :two-pairs 2 :one-pair 1 :high-card 0})

(defn- hand-to-hand-strength
  [hand]
  (let [cards-with-count (frequencies hand)
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
    {:hand hand :hand-strength (hand-to-hand-strength hand) :bid bid}))

(defn- compare-hands
  [a b card-to-strength-map]
  (let [[a-card b-card & _] (->>
                             (map vector a b)
                             (drop-while #(apply = %))
                             (first))]
   (compare (get card-to-strength-map a-card) (get card-to-strength-map b-card))))

(defn- compare-hands-and-bids
  [a b card-to-strength-map]
  (let [a-strength ((:hand-strength a) hand-strength-map)
        b-strength ((:hand-strength b) hand-strength-map)
        c (compare a-strength b-strength)]
    (if (not= c 0)
      c
      (compare-hands (:hand a) (:hand b) card-to-strength-map))))

(defn- calculate-total-winnings
  [card-to-strength-map-fun map-jokers-fun]
  (->>
   (input/compose-input-filename "day_seven")
   (input/read-file-into-vector)
   (map parse-line)
   (map map-jokers-fun)
   (sort #(compare-hands-and-bids %1 %2 card-to-strength-map-fun))
   (map-indexed (fn [i {bid :bid}] (* (inc i) bid)))
   (apply +)))

(defn part-one
  []
  (calculate-total-winnings card-to-strength-map (fn [x] x)))

(defn- joker?
  [c]
  (= \J c))

(defn- map-jokers
  [{:keys [hand hand-strength bid]}]
  {:hand hand
   :bid bid
   :hand-strength (if (some joker? hand)
                    (cond
                      (= hand-strength :high-card)        :one-pair
                      (= hand-strength :one-pair)         :three-of-a-kind
                      (= hand-strength :two-pairs)        (if (= 2 (get (frequencies hand) \J)) :four-of-a-kind :full-house)
                      (= hand-strength :three-of-a-kind)  :four-of-a-kind
                      (= hand-strength :full-house)       :five-of-a-kind
                      (= hand-strength :four-of-a-kind)   :five-of-a-kind
                      :else #_:five-of-a-kind             hand-strength)
                    hand-strength)})

(defn part-two
  []
  (calculate-total-winnings joker-card-to-strength-map map-jokers))