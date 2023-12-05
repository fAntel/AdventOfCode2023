(ns advent-of-code-2023.core
  (:require [;advent-of-code-2023.days.day-one :as day-one
              advent-of-code-2023.days.day-two :as day-two])
  (:gen-class))

(defn -main
  [& args]
  (println (day-two/part-one))
  (println (day-two/part-two))
  0)