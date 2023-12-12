(ns advent-of-code-2023.days.day-four
  (:require [advent-of-code-2023.utils.input-parsers :as input]
            [clojure.string :as str]))

(defn- power-of-two
  ([power]
   (if (zero? power)
     0
     (power-of-two 1 (dec power))))
  ([result counter]
   (if (zero? counter)
     result
     (power-of-two (* result 2) (dec counter)))))

(defn- count-points-in-card
  [winning-numbers numbers]
  (let [min-winning-number (first winning-numbers)
        max-winning-number (last winning-numbers)
        count-winning-in-numbers-reducer (fn
                                           [acc number]
                                           (cond
                                            (< number min-winning-number) acc
                                            (> number max-winning-number) acc
                                            (some #{number} winning-numbers) (inc acc)
                                            :else acc))
        count-winning-in-numbers (reduce #(count-winning-in-numbers-reducer %1 %2) 0 numbers)]
    (power-of-two count-winning-in-numbers)))

(defn- parse-card-info
  [line]
  (let [card-info-regex #"Card\s*(?<cardNumber>\d+):\s+(?<winningNumbers>.+)\s+\|\s+(?<numbersElfHave>.+)"
        matcher (re-matcher card-info-regex line)
        _ (re-find matcher)
        card-number (.group matcher "cardNumber")
        winning-numbers-as-astr (.group matcher "winningNumbers")
        numbers-as-str (.group matcher "numbersElfHave")
        numbers-parsing-fun (fn [s dst] (reduce #(conj %1 (Integer/parseInt (str/trim %2))) dst (str/split s #"\s+")))
        winning-numbers (numbers-parsing-fun winning-numbers-as-astr (sorted-set))
        numbers (numbers-parsing-fun numbers-as-str (vector))]
    {:card-number card-number :winning-numbers winning-numbers :numbers numbers}))

(defn part-one
  []
  (input/process-file-by-lines
   (input/compose-input-filename "day_four")
   #(let [card-info (parse-card-info %)] (count-points-in-card (:winning-numbers card-info) (:numbers card-info)))
   +))

(defn part-two
  []
  nil)