(require '[clojure.string :as str])
(require 'sc.api)

(def input_string (slurp "d6.txt"))
(def grid (str/split input_string #"\n"))
(defn get_starting_location [grid]
  (let [rows (count grid)
        cols (count (first grid))
        starting_pos (first (for [
                           i (range rows) 
                           j (range cols) 
                           :when (= (get-in grid [i, j]) \^)] 
                       [i j]))
        ] starting_pos
   ))
(def start (get_starting_location grid))
(defn in_bounds? [i, j, grid] 
  (let [rows (count grid)
        cols (count (first grid))
        ibounds? (and (>= i 0) (< i rows) (>= j 0) (< j cols))
        ]
   ibounds?))
start
