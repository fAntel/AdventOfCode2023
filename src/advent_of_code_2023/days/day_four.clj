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

(defn- count-winning-numbers-count-in-card
  [winning-numbers numbers]
  (let [min-winning-number (first winning-numbers)
        max-winning-number (last winning-numbers)
        reducer            (fn
                             [acc number]
                             (cond
                              (< number min-winning-number)    acc
                              (> number max-winning-number)    acc
                              (some #{number} winning-numbers) (inc acc)
                              :else                            acc))]
    (reduce #(reducer %1 %2) 0 numbers)))

(defn- parse-card-info
  [line]
  (let [card-info-regex         #"Card\s*(?<cardNumber>\d+):\s+(?<winningNumbers>.+)\s+\|\s+(?<numbersElfHave>.+)"
        matcher                 (re-matcher card-info-regex line)
        _                       (re-find matcher)
        card-number             (Integer/parseInt (.group matcher "cardNumber"))
        winning-numbers-as-astr (.group matcher "winningNumbers")
        numbers-as-str          (.group matcher "numbersElfHave")
        numbers-parsing-fun     (fn [s dst]
                                  (reduce #(conj %1 (Integer/parseInt (str/trim %2))) dst (str/split s #"\s+")))
        winning-numbers         (numbers-parsing-fun winning-numbers-as-astr (sorted-set))
        numbers                 (numbers-parsing-fun numbers-as-str (vector))]
    {:card-number card-number :winning-numbers winning-numbers :numbers numbers}))

(defn part-one
  []
  (input/process-file-by-lines
   (input/compose-input-filename "day_four")
   (fn [line]
     (let [card-info (parse-card-info line)
           count     (count-winning-numbers-count-in-card (:winning-numbers card-info) (:numbers card-info))]
       (power-of-two count)))
   +))

(defn- prepare-cards
  [cards-info]
  (into (sorted-map)
        (mapcat
         (fn [card-into]
           (let [winning-numbers (:winning-numbers card-into)
                 numbers         (:numbers card-into)
                 points          (count-winning-numbers-count-in-card winning-numbers numbers)]
             {(:card-number card-into) {:points points :copies 1}}))
         cards-info)))

(defn- add-copies
  [result next-card-number counter copies]
  (if (and (pos? counter) (contains? result next-card-number))
    (let [next-card-data (get result next-card-number)
          old-copies     (:copies next-card-data)
          new-card-data  (assoc next-card-data :copies (+ old-copies copies))]
      (recur
        (assoc result next-card-number new-card-data)
        (inc next-card-number)
        (dec counter)
        copies))
    result))

(defn- get-cards-for-winning-numbers
  [result card-number]
  (if (contains? result card-number)
    (let [card-data        (get result card-number)
          next-card-number (inc card-number)]
      (recur
        (add-copies result next-card-number (:points card-data) (:copies card-data))
        next-card-number))
    result))

(defn part-two
  []
  (let [cards-as-str      (input/read-file-into-vector (input/compose-input-filename "day_four"))
        cards-info        (map #(parse-card-info %) cards-as-str)
        cards-with-points (prepare-cards cards-info)
        resulted-cards    (get-cards-for-winning-numbers cards-with-points (key (first cards-with-points)))]
    (reduce-kv
     (fn [acc _ card-data]
       (+ acc (:copies card-data)))
     0
     resulted-cards)))