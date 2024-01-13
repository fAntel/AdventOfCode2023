(ns advent-of-code-2023.days.day-eleven
  (:require [advent-of-code-2023.utils.input-parsers :as input]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [clojure.set]))

(defn- universe-to-list-of-galaxies
  ([universe]
   (let [height (count universe)
         width  (count (first universe))]
     (universe-to-list-of-galaxies universe height width)))
  ([universe height width]
   (reduce
    (fn [acc y]
      (reduce
       (fn [acc x]
         (if (= \# (nth (nth universe y) x))
           (conj acc {:x x :y y})
           acc))
       acc
       (range width)))
    #{}
    (range height))))

(defn- lines-to-expand
  [galaxies key line-size]
  (clojure.set/difference (into (sorted-set) (range line-size)) (set (map key galaxies))))

(defn- expand-universe
  [universe]
  (let [height            (count universe)
        width             (count (first universe))
        galaxies          (universe-to-list-of-galaxies universe height width)
        columns-to-expand (lines-to-expand galaxies :x width)
        rows-to-expand    (lines-to-expand galaxies :y height)]
    (reduce
     (fn [acc {:keys [x y]}]
       (conj acc
             {:x (+ x (count (subseq columns-to-expand < x)))
              :y (+ y (count (subseq rows-to-expand < y)))}))
     #{}
     galaxies)))

(defn- calculate-taxicab-geometry-distance
  [from to]
  (+ (abs (- (:x from) (:x to))) (abs (- (:y from) (:y to)))))

(defn- find-all-path-lengthes
  [galaxies]
  (->>
   (combo/combinations galaxies 2)
   (map #(calculate-taxicab-geometry-distance (first %) (second %)))))

(defn part-one
  []
  (->>
   (input/compose-input-filename "day_eleven")
   (input/read-file-into-vector)
   (expand-universe)
   (find-all-path-lengthes)
   (apply +)))

(defn part-two
  []
  nil)