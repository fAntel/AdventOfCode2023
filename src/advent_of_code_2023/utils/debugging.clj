(ns advent-of-code-2023.utils.debugging)

(defn print-map
  [map-to-print]
  (println
   (str
    (reduce
     (fn [^StringBuilder acc row]
       (reduce #(.append %1 %2) acc row)
       (.append acc "\n")
       acc)
     (StringBuilder.)
     map-to-print)))
  map-to-print)