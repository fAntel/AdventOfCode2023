(ns advent-of-code-2023.days.day-one
  (:require [advent-of-code-2023.utils.input-parsers :as input])
  (:gen-class))

(def find-first-digit
  (fn this
    ([s from-index step]
     (let
       [x (nth s from-index)
         d (Character/digit x 10)]
       (if (>= d 0)
         d
         (recur s (+ from-index step) step))))
    ([s from-index]
     (this s from-index 1))
    ([s]
     (this s 0 1))))

(defn find-calibration-value
  [line]
  (let
    [a (char-array line)]
    (+ (* (find-first-digit a) 10) (find-first-digit a (dec (count line)) -1))))

(defn part-one
  []
  (input/process-file-by-lines (input/create-input-file "day_one") #(find-calibration-value %) +))