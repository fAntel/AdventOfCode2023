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

(defn- filter-possible-digits-into-digits-with-coords
  [map-as-coords possible-digits-coords]
  (reduce
   (fn
     [acc [coord char]]
     (let [d (Character/digit char 10)]
       (if
         (and (>= d 0) (contains? possible-digits-coords coord))
         (conj acc {coord d})
         acc)))
   {}
   map-as-coords))

(defn- digits-by-line-reducer
  [acc coord d]
  (let [x (get coord :x)
        y (get coord :y)
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
        map-as-coords (map-to-coords map-as-lines)
        symbols (into {} (filter (fn [[coord char]] (and (not= \. char) (neg? (Character/digit char 10)))) map-as-coords))
        possible-digits-coords (set (mapcat (fn [[coord char]] (coord-to-possible-coords-around-a-symbol coord width height)) symbols))
        digits (filter-possible-digits-into-digits-with-coords map-as-coords possible-digits-coords)
        digits-by-line (reduce-kv digits-by-line-reducer {} digits)
        numbers (digits-to-numbers digits-by-line map-as-lines)]
    numbers))

(defn- filter-possible-digits-coords-around-a-gear
  [map-as-coords coords]
  (reduce
   (fn [acc coord]
     (let [c (get map-as-coords coord)
           d (Character/digit c 10)]
       (if (>= d 0)
         (conj acc {coord d})
         acc)))
   {}
   coords))

(defn- calculate-gear-ration-if-possible
  [gear-coord map-as-lines map-as-coords width height]
  (let [possible-digits-coords (coord-to-possible-coords-around-a-symbol gear-coord width height)
        digits-coords          (filter-possible-digits-coords-around-a-gear map-as-coords possible-digits-coords)
        digits-by-line (reduce-kv digits-by-line-reducer {} digits-coords)
        numbers-for-possible-grear (digits-to-numbers digits-by-line map-as-lines)]
    (if (= (count numbers-for-possible-grear) 2) (apply * numbers-for-possible-grear))))

(defn- find-gear-ratios
  [map-as-lines]
  (let [width (count (first map-as-lines))
        height (count map-as-lines)
        map-as-coords (map-to-coords map-as-lines)
        possible-gears (filter (fn [[coord char]] (= char \*)) map-as-coords)
        gear-ratios (map (fn [[coord _]] (calculate-gear-ration-if-possible coord map-as-lines map-as-coords width height)) possible-gears)]
    (remove nil? gear-ratios)))

(defn part-one
  []
  (let [map-as-lines (input/read-file-into-vector (input/compose-input-filename "day_three"))]
    (apply + (find-part-numbers map-as-lines))))

(defn part-two
  []
  (let [map-as-lines (input/read-file-into-vector (input/compose-input-filename "day_three"))]
    (apply + (find-gear-ratios map-as-lines))))