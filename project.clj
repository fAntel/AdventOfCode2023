(defproject advent-of-code-2023 "0.0.0"
  :description "My Advent of Code 2023 solutions in pursuit to learn Clojure."
  :url "https://github.com/fAntel/AdventOfCode2023"
  :license {:name "BSD-3-Clause"}
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/math.combinatorics "0.2.0"]]
  :main ^:skip-aot advent-of-code-2023.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
