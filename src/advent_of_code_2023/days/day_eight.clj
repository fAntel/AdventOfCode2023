(ns advent-of-code-2023.days.day-eight
  (:require [advent-of-code-2023.utils.input-parsers :as input]
            [clojure.string :as str]))

(defn- parse-network-str
  [network-as-str]
  (let [regex   #"(?<nodeName>[A-Z]+)\s*=\s*\((?<leftNode>[A-Z]+),\s*(?<rightNode>[A-Z]+)\)"
        matcher (re-matcher regex network-as-str)
        _       (re-find matcher)]
    {(.group matcher "nodeName") {:L (.group matcher "leftNode") :R (.group matcher "rightNode")}}))

(defn- parse-input
  [[instructions _ & network]]
  {:instructions (map #(if (= \R %) :R :L) (seq instructions))
   :network (reduce #(conj %1 (parse-network-str %2)) {} network)})

(defn- calculate-route-duration
  [{:keys [instructions network]}]
  (loop [current-node      "AAA"
         rest-instructions instructions
         duration          0]
    (if (= current-node "ZZZ")
      duration
      (recur
        ((first rest-instructions) (get network current-node))
        (let [new-rest-instrucitons (drop 1 rest-instructions)]
          (if (empty? new-rest-instrucitons) instructions new-rest-instrucitons))
        (inc duration)))))

(defn part-one
  []
  (->>
   (input/compose-input-filename "day_eight")
   (input/read-file-into-vector)
   (parse-input)
   (calculate-route-duration)))

(defn part-two
  []
  nil)