(ns advent-of-code-2023.days.day-three
  (:require [advent-of-code-2023.utils.input-parsers :as input]
            [clojure.string :as str]))

(defn- map-to-coords
  [map-as-lines]
  (let [line-to-coords (fn [line y] (map-indexed (fn [x c] {{:x x :y y} c}) line))
        list-coords-by-line (fn [map] (map-indexed (fn [y line] (line-to-coords line y)) map))]
    (into {} (flatten (list-coords-by-line map-as-lines)))))

(defn- coord-to-possible-coords-around-a-symbol
  [coord width height]
  (let [x (get coord :x)
        y (get coord :y)
        max-x (dec width)
        max-y (dec height)
        coords-for-y (fn [x y] (set (map (fn [x] {:x x :y y}) (range (max (dec x) 0) (inc (min (inc x) max-x))))))]
    (concat (if (> x 0) #{{:x (dec x) :y y}} #{})
            (if (< x max-x) #{{:x (inc x) :y y}} #{})
            (if (> y 0) (coords-for-y x (dec y)) #{})
            (if (< y max-y) (coords-for-y x (inc y)) #{}))))

(defn- digits-by-line-reducer
  [acc coord char]
  (let [x (get coord :x)
        y (get coord :y)
        d (Character/digit char 10)
        chars (get acc y [])]
    (assoc acc y (conj chars {:x x :d d}))))

(defn- compose-number
  ([s d x]
   (let [builder (StringBuilder.)
         from (compose-number s (dec x) -1 builder)
         _ (.reverse builder)
         _ (.append builder d)
         to (compose-number s (inc x) 1 builder)
         number (Integer/parseInt (str builder))]
     {:number number :range {:from from :to to}}))
  ([s i step builder]
   (let [c (get s i)
         d (if (not (nil? c)) (Character/digit c 10))]
     (if (and (not (nil? d)) (>= d 0))
       (do
         (.append builder d)
         (compose-number s (+ i step) step builder))
       (- i step)))))

(defn- digit-to-number-reducer
  [data map-as-lines line-number]
  (reduce
   (fn [acc digit]
     (let [ranges  (get acc :ranges)
           numbers (get acc :numbers)
           x       (get digit :x)
           d       (get digit :d)
           s       (get map-as-lines line-number)
           in      (fn [range i] (let [from (get range :from) to (get range :to)] (<= from i to)))]
       (if (some #(in % x) ranges)
         acc
         (let [result (compose-number s d x)
               number (get result :number)
               range  (get result :range)]
           (assoc acc :ranges (conj ranges range) :numbers (conj numbers number))))))
   {:ranges [] :numbers []}
   data))

(defn- digits-to-numbers
  [digits-by-line map-as-lines]
  (reduce-kv
   (fn [acc line-number data]
     (into acc (get (digit-to-number-reducer data map-as-lines line-number) :numbers)))
   []
   digits-by-line))

(defn- find-part-numbers
  [map-as-lines]
  (let [width (count (first map-as-lines))
        height (count map-as-lines)
        ;_ (printf "width: %d, height: %d\n" width height)
        map-as-coords (map-to-coords map-as-lines)
        symbols (into {} (filter (fn [[coord char]] (and (not= \. char) (neg? (Character/digit char 10)))) map-as-coords))
        ;_ (printf "symbols:\n%s\n" symbols)
        possible-digits-coords (set (mapcat (fn [[coord char]] (coord-to-possible-coords-around-a-symbol coord width height)) symbols))
        ;_ (printf "possible-digits-coords:\n%s\n" possible-digits-coords)
        digits (into {} (filter (fn [[coord char]] (and (>= (Character/digit char 10) 0) (contains? possible-digits-coords coord))) map-as-coords))
        ;_ (printf "digits:\n%s\n" digits)
        ;_ (printf "digits size: %d\n" (count digits))
        digits-by-line (reduce-kv digits-by-line-reducer {} digits)
        _ (printf "digits-by-line:\n%s\n" digits-by-line)
        numbers (digits-to-numbers digits-by-line map-as-lines)
        _ (printf "numbers:\n%s\n" numbers)]
    numbers))

(defn part-one
  []
  (let [map-as-lines (input/read-file-into-vector (input/compose-input-filename "day_three"))]
    (apply + (find-part-numbers map-as-lines))))

(defn part-two
  []
  nil)