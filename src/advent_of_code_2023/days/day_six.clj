(ns advent-of-code-2023.days.day-six
  (:require [advent-of-code-2023.utils.input-parsers :as input]
            [clojure.string :as str]))

(defn- parse-line
  [line]
  (->>
   (str/split (str/trim line) #"\s+")
   (drop 1)
   (map #(Long/parseLong %))))

(defn- parse-input-as-list-of-records
  [lines]
  (map (fn [time distance] {:time time :distance distance}) (parse-line (first lines)) (parse-line (last lines))))

(defn- list-into-long
  [numbers]
  (Long/parseLong (apply str numbers)))

(defn- parse-input-as-a-record
  [lines]
  {:time (list-into-long (parse-line (first lines)))
   :distance (list-into-long (parse-line (last lines)))})

(defn- find-new-record-index
  [cur-speed max-speed distance-record time-step-fun]
  (loop [cur-speed cur-speed]
    (let [rest-duration (- max-speed cur-speed)
          distance (* cur-speed rest-duration)]
      (if (> distance distance-record)
        cur-speed
        (recur (time-step-fun cur-speed))))))

(defn- find-new-record
  [{:keys [time distance]}]
  (range
   (find-new-record-index 1 time distance inc)
   (inc (find-new-record-index (dec time) time distance dec))))

(defn part-one
  []
  (->>
   (input/compose-input-filename "day_six")
   (input/read-file-into-vector)
   (parse-input-as-list-of-records)
   (map #(find-new-record %))
   (map count)
   (apply *)))

(defn part-two
  []
  (->>
   (input/compose-input-filename "day_six")
   (input/read-file-into-vector)
   (parse-input-as-a-record)
   (find-new-record)
   (count)))