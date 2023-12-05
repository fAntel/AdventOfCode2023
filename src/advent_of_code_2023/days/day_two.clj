(ns advent-of-code-2023.days.day-two
  (:require [advent-of-code-2023.utils.input-parsers :as input]
            [clojure.string :as str])
  (:gen-class))

(def game-prefix-length 5)

(def default-bag-content {"red" 12 "green" 13 "blue" 14})

(defn get-game-number
  [game-lexem]
  (Integer/parseInt (apply str (drop game-prefix-length game-lexem))))

(defn try-to-cubes-list
  [try-as-str]
  (let [cubes-as-strs (str/split (str/trim try-as-str) #",")
        cubes-as-pairs (map #(str/split (str/trim %) #" ") cubes-as-strs)]
    (map (fn [[count colour]] {:colour colour :count (Integer/parseInt count)}) cubes-as-pairs)))

(defn try-to-cubes
  [try-as-str]
  (let [cubes-list (try-to-cubes-list try-as-str)]
    (reduce (fn [acc entry] (assoc acc (:colour entry) (+ (:count entry) (get acc (:colour entry) 0)))) {} cubes-list)))

(defn possible-cubes?
  [[colour count & _]]
  (and (contains? default-bag-content colour) (<= count (get default-bag-content colour))))

(defn possible-try?
  [try]
  (let [cubes (try-to-cubes try)]
    (every? #(possible-cubes? %) cubes)))

(defn possible-game?
  [tries]
  (every? #(possible-try? %) tries))

(defn parse-game-info
  [line]
  (let [[game-lexem tries-lexem] (str/split line #":")
        game-number (get-game-number game-lexem)
        tries (str/split tries-lexem #";")]
    {:game-number game-number :tries tries}))

(defn game-to-number
  [game-number tries]
  (if (possible-game? tries) game-number 0))

(defn least-possible-set-reduce-fun
  [acc entry]
  (let [colour (:colour entry)
        count (:count entry)]
    (assoc acc colour (max count (get acc colour 0)))))

(defn power-of-game
  [tries]
  (let [cubes (flatten (map try-to-cubes-list tries))
        least-possible-set (reduce least-possible-set-reduce-fun {} cubes)]
    (reduce-kv (fn [acc k v] (if (zero? acc) v (* acc v))) 0 least-possible-set)))

(defn part-one
  []
  (input/process-file-by-lines
   (input/compose-input-filename "day_two")
   #(let [game-info (parse-game-info %)] (game-to-number (:game-number game-info) (:tries game-info)))
   +))

(defn part-two
  []
  (input/process-file-by-lines
   (input/compose-input-filename "day_two")
   #(let [game-info (parse-game-info %)] (power-of-game (:tries game-info)))
   +))