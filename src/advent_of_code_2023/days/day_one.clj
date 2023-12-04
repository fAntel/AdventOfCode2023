(ns advent-of-code-2023.days.day-one
  (:require [advent-of-code-2023.utils.input-parsers :as input])
  (:gen-class))

(def digits-as-str {1 "one", 2 "two", 3 "three", 4 "four", 5 "five", 6 "six", 7 "seven", 8 "eight", 9 "nine"})

(def reversed-digits-as-str (into {} (map (fn [[k v]] {k (clojure.string/reverse v)}) digits-as-str)))

(defn find-digit
  ([s from-index step]
   (let [x (nth s from-index)
         d (Character/digit x 10)]
     (if (>= d 0)
       d
       (recur s (+ from-index step) step))))
  ([s from-index step lexems-map]
   (if (empty? lexems-map)
     (find-digit s from-index step)
     (let [inc-possble-lexems-indexes (fn [ls] (into {} (map (fn [[k [v i]]] {k [v (inc i)]}) ls)))
           lexem-found?               (fn [ls] (and (= 1 (count ls)) (let [[k [v i]] (first ls)] (= (count v) (inc i)))))
           default-possible-lexems    (into {} (map (fn [[k v]] {k [v 0]}) lexems-map))]
       (loop [s s i from-index step step lexems default-possible-lexems]
         (let [x (nth s i)
               d (Character/digit x 10)]
           (if (>= d 0)
             d
             (let [rest-possible-lexems         (into {} (filter (fn [[k [v i]]] (= x (nth v i))) lexems))
                   rest-default-possible-lexems (into {} (filter (fn [[k [v i]]] (= x (nth v i))) default-possible-lexems))
                   ls                           (merge rest-default-possible-lexems rest-possible-lexems)
                   found-lexems                 (into {} (filter (fn [[k [v i]]] (= (count v) (inc i))) ls))]
               (cond
                 (not (empty? found-lexems)) (key (first found-lexems))
                 (not (empty? ls))           (recur s (+ i step) step (inc-possble-lexems-indexes ls))
                 :else                       (recur s (+ i step) step default-possible-lexems))))))))))

(def find-first-digit
  (fn this
    ([s from-index step]
     (find-digit s from-index step))
    ([s from-index]
     (this s from-index 1))
    ([s]
     (this s 0 1))))

(def find-last-digit
  (fn this
    ([s from-index step]
     (find-digit s from-index step))
    ([s from-index]
     (this s from-index -1))
    ([s]
     (this s (dec (count s)) -1))))

(defn find-calibration-value
  [line first-digin-fun last-digit-fun]
  (+ (* (first-digin-fun line) 10) (last-digit-fun line)))

(defn part-one
  []
  (input/process-file-by-lines
   (input/create-input-file "day_one")
   #(find-calibration-value % find-first-digit find-last-digit)
   +))

(defn part-two
  []
  (input/process-file-by-lines
   (input/create-input-file "day_one")
   #(find-calibration-value
     %
     (fn [s] (find-digit s 0 1 digits-as-str))
     (fn [s] (find-digit s (dec (count s)) -1 reversed-digits-as-str)))
   +))