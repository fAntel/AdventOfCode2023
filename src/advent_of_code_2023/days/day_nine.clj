(ns advent-of-code-2023.days.day-nine
  (:require [advent-of-code-2023.utils.input-parsers :as input]
            [clojure.string :as str]))

(defn- parse-line
  [line]
  (map parse-long (str/split line #"\s+")))

(defn- extrapolate
  [values]
  (if (every? zero? values)
    (last values)
    (+ (last values) (extrapolate (map #(- (last %) (first %)) (partition 2 1 values))))))

(defn- extrapolate-backward
  [values]
  (if (every? zero? values)
    (first values)
    (- (first values) (extrapolate-backward (map #(- (last %) (first %)) (partition 2 1 values))))))

(defn- find-extrapolated-value
  [line extrapolate-fun]
  (extrapolate-fun (parse-line line)))

(defn part-one
  []
  (input/process-file-by-lines
   (input/compose-input-filename "day_nine")
   #(find-extrapolated-value % extrapolate)
   +))

(defn part-two
  []
  (input/process-file-by-lines
   (input/compose-input-filename "day_nine")
   #(find-extrapolated-value % extrapolate-backward)
   +))