(ns advent-of-code-2023.days.day-two
  (:require [advent-of-code-2023.utils.input-parsers :as input]
            [clojure.string :as str])
  (:gen-class))

(def game-prefix-length 5)

(def default-bag-content {"red" 12 "green" 13 "blue" 14})

(defn get-game-number
  [game-lexem]
  (Integer/parseInt (apply str (drop game-prefix-length game-lexem))))

(defn try-to-cubes
  [try-as-str]
  (let [cubes-as-strs (str/split try-as-str #",")
        cubes-as-pairs (map #(str/split (str/trim %) #" ") cubes-as-strs)
        cubes-list (map (fn [[count colour]] {:colour colour :count (Integer/parseInt count)}) cubes-as-pairs)]
    (reduce (fn [acc entry] (assoc acc (:colour entry) (+ (:count entry) (get acc (:colour entry) 0)))) {} cubes-list)))

(defn possible-cubes?
  [[colour count & _]]
  (and (contains? default-bag-content colour) (<= count (get default-bag-content colour))))

(defn possible-try?
  [try]
  (let [cubes (try-to-cubes (str/trim try))]
    (every? #(possible-cubes? %) cubes)))

(defn possible-game?
  [tries]
  (every? #(possible-try? %) tries))

(defn game-to-number
  [line]
  (let [[game-lexem tries-lexem] (str/split line #":")
        game-number (get-game-number game-lexem)
        tries (str/split tries-lexem #";")]
    (if (possible-game? tries) game-number 0)))

(defn part-one
  []
  (input/process-file-by-lines
   (input/compose-input-filename "day_two")
   #(game-to-number %)
   +))

(defn part-two
  []
  nil)