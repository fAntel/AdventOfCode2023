(ns advent-of-code-2023.utils.input-parsers
  (:require [clojure.java.io :as io])
  (:gen-class))

(def days-parent-path "src/advent_of_code_2023/days")

(defn create-input-file [day-name]
  (io/file days-parent-path (str day-name "_input")))

(defn process-file-by-lines
  [file process-fun output-fun]
  (with-open [reader (io/reader file)]
    (apply output-fun (map process-fun (line-seq reader)))))