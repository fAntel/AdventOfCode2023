(ns advent-of-code-2023.days.day-ten
  (:require [advent-of-code-2023.utils.input-parsers :as input]
            [clojure.string :as str]))

(defn- left-pipe?
  [c]
  (or (= c \-) (= c \L) (= c \F)))

(defn- right-pipe?
  [c]
  (or (= c \-) (= c \J) (= c \7)))

(defn- top-pipe?
  [c]
  (or (= c \|) (= c \7) (= c \F)))

(defn- bottom-pipe?
  [c]
  (or (= c \|) (= c \L) (= c \J)))

(defn- find-starting-position
  [lines]
  (loop [y 0]
    (let [x (str/index-of (nth lines y) \S)]
      (if x
        {:x x :y y}
        (recur (inc y))))))

(defn- get-pipe
  [lines x y]
  (nth (nth lines y) x))

(defn- find-directions-for-starting-position
  [lines height width {:keys [x y]}]
  (concat
   (if (and (> x 0) (left-pipe? (get-pipe lines (dec x) y)))
     #{{:x (dec x) :y y :from :east}})
   (if (and (< x (dec width)) (right-pipe? (get-pipe lines (inc x) y)))
     #{{:x (inc x) :y y :from :west}})
   (if (and (> y 0) (top-pipe? (get-pipe lines x (dec y))))
     #{{:x x :y (dec y) :from :south}})
   (if (and (< y (dec height)) (bottom-pipe? (get-pipe lines x (inc y))))
     #{{:x x :y (inc y) :from :north}})))

(defn- find-next-pipe
  [lines {:keys [x y from]}]
  (let [pipe (get-pipe lines x y)]
    (cond
     (= from :east)  (cond
                      (= pipe \-)         {:x (dec x) :y y :from :east}
                      (= pipe \L)         {:x x :y (dec y) :from :south}
                      :else #_(= pipe \F) {:x x :y (inc y) :from :north})
     (= from :west)  (cond
                      (= pipe \-)         {:x (inc x) :y y :from :west}
                      (= pipe \J)         {:x x :y (dec y) :from :south}
                      :else #_(= pipe \7) {:x x :y (inc y) :from :north})
     (= from :north) (cond
                      (= pipe \|)         {:x x :y (inc y) :from :north}
                      (= pipe \L)         {:x (inc x) :y y :from :west}
                      :else #_(= pipe \J) {:x (dec x) :y y :from :east})
     :else #_(= from :south) (cond
                              (= pipe \|)         {:x x :y (dec y) :from :south}
                              (= pipe \7)         {:x (dec x) :y y :from :east}
                              :else #_(= pipe \F) {:x (inc x) :y y :from :west}))))

(defn- same-coords?
  [coords]
  (let [a (first coords)
        b (second coords)]
    (and (= (:x a) (:x b)) (= (:y a) (:y b)))))

(defn- calculate-steps-to-farthest-position
  [lines]
  (let [height            (count lines)
        width             (count (first lines))
        starting-position (find-starting-position lines)
        directions        (find-directions-for-starting-position lines height width starting-position)]
    (loop [steps-count 1
           directions  directions]
      (if (same-coords? directions)
        steps-count
        (recur (inc steps-count) (map #(find-next-pipe lines %) directions))))))

(defn part-one
  []
  (->>
   (input/compose-input-filename "day_ten")
   (input/read-file-into-vector)
   (calculate-steps-to-farthest-position)))

(defn part-two
  []
  nil)