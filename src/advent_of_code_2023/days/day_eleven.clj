(ns advent-of-code-2023.days.day-eleven
  (:require [advent-of-code-2023.utils.input-parsers :as input]
            [advent-of-code-2023.utils.debugging :as debugging]
            [clojure.string :as str]
            [clojure.math.combinatorics :as combo]))

(defn- get-column
  [lines x height]
  (map (fn [y] (nth (nth lines y) x)) (range height)))

(defn- find-line-to-expand
  [line-getter lines-count]
  (->>
   (range lines-count)
   (map (fn [line-number] {:line-number line-number :line (line-getter line-number)}))
   (filter (fn [{:keys [line]}] (apply = \. line)))
   (map #(get % :line-number))
   (sort)))

(defn- lines-to-universe
  [lines]
  (flatten
   (reduce-kv
    (fn [acc y line]
      (conj acc (map-indexed (fn [x c] {{:x x :y y} c}) line)))
    {}
    lines)))

; TODO we don't release need full map. This function can be optimized if we find only galaxies with theirs coordinates
; TODO and then increase x/y to expand.
(defn- expand-universe
  [lines]
  (let [height            (count lines)
        width             (count (first lines))
        columns-to-expand (find-line-to-expand #(get-column lines % height) width)
        rows-to-expand    (find-line-to-expand #(nth lines %) height)
        empty-column      (repeat height \.)
        expanded-columns  (reduce
                           (fn [acc x]
                             (if (some #{x} columns-to-expand)
                               (conj acc empty-column empty-column)
                               (conj acc (get-column lines x height))))
                           []
                           (range width))
        empty-row          (repeat (count expanded-columns) \.)
        expanded-universe  (reduce
                            (fn [acc y]
                              (if (some #{y} rows-to-expand)
                                (conj acc empty-row empty-row)
                                (conj acc (map #(nth % y) expanded-columns))))
                            []
                            (range height))]
    expanded-universe))

(defn- universe-to-list-of-galaxies
  [universe]
  (let [height (count universe)
        width  (count (first universe))]
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

(defn- find-path-length
  [from to]
  (cond
    (= (:x from) (:x to)) (abs (- (:y to) (:y from)))
    (= (:y from) (:y to)) (abs (- (:x to) (:x from)))
    :else                 (loop [cur  {:x (min (:x from) (:x to)) :y (min (:y from) (:y to))}
                                 dest {:x (max (:x from) (:x to)) :y (max (:y from) (:y to))}
                                 length 0]
                            (if (= cur dest)
                              length
                              (recur
                                (let [cur-x  (:x cur)
                                      cur-y  (:y cur)
                                      x-diff (- (:x dest) cur-x)
                                      y-diff (- (:y dest) cur-y)]
                                  (if (> y-diff x-diff)
                                    {:x cur-x :y (inc cur-y)}
                                    {:x (inc cur-x) :y cur-y}))
                                dest
                                (inc length))))))

(defn- find-all-path-lengthes
  [galaxies]
  (->>
   (combo/combinations galaxies 2)
   (map #(find-path-length (first %) (second %)))))

(defn part-one
  []
  (->>
   (input/compose-input-filename "day_eleven")
   (input/read-file-into-vector)
   (expand-universe)
   ;(debugging/print-map)
   (universe-to-list-of-galaxies)
   (find-all-path-lengthes)
   (apply +)))

(defn part-two
  []
  nil)