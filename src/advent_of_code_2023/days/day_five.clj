(ns advent-of-code-2023.days.day-five
  (:require [advent-of-code-2023.utils.input-parsers :as input]
            [clojure.string :as str]))

(defn- line-to-mapping-fun
  [line]
  (let [[dst src len] (map #(Long/parseLong (str/trim %)) (str/split line #"\s"))
        from src
        to (dec (+ src len))]
    (fn [x]
      (if (and (>= x from) (<= x to))
        (+ dst (- x from))))))

(defn- parse-almanac
  [lines]
  (let [seeds (map #(Long/parseLong (str/trim %)) (drop 1 (str/split (first lines) #"\s")))
        mappings (partition-by str/blank? (drop 2 lines))
        mappings (map #(map (fn [line] (line-to-mapping-fun line)) (drop 1 %)) mappings)]
    {:seeds seeds :mappings mappings}))

(defn- find-localtions
  [seeds mappings]
  (into {}
        (map
         (fn [seed]
           {(reduce (fn [acc mapping] (or (first (drop-while nil? (map #(% acc) mapping))) acc)) seed mappings)
            seed})
         seeds)))

(defn part-one
  []
  (let [lines (input/read-file-into-vector (input/compose-input-filename "day_five"))
        almanac (parse-almanac lines)
        seeds (:seeds almanac)
        mappings (:mappings almanac)
        locations (find-localtions seeds mappings)]
    (apply min (keys locations))))

(defn part-two
  []
  nil)