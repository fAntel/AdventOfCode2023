(ns advent-of-code-2023.core
  (:require [;advent-of-code-2023.days.day-one :as day-one
             ;advent-of-code-2023.days.day-two :as day-two
              advent-of-code-2023.days.day-three :as day-three])
  (:gen-class))

(defn -main
  [& args]
  (println (day-three/part-one))
  (println (day-three/part-two))
  0)