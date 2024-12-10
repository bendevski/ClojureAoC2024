(require '[clojure.string :as str])
(require 'sc.api)

(def input_string (slurp "d8f.txt"))
(defn format_input [input_string]
  (let [string_grid (str/split input_string #"\n")
        char_list_grid (mapv #(mapv char %) string_grid)
        ]
   char_list_grid))
(defn add_to_or_create_map_list [acc [k v]] (update acc k #(conj (or % []) v)))
(def grid (format_input input_string))
(defn get_antennae_map [grid] 
  (reduce 
   add_to_or_create_map_list 
   {} 
   (for [i (range (count grid))
         j (range (count (first grid)))
         :when (not= (get-in grid [i j]) \.)] [(get-in grid [i j]) [i j]]
        )))
(def antennae_map (get_antennae_map grid))
(def antennae_vec (vec (vals antennae_map)))
(defn antinodes_for_two_antennaes [[i1 j1], [i2 j2]] 
  (let [diffi (- i1 i2)
        diffj (- j1 j2)
        a_n1 [(+ i1 diffi) (+ j1 diffj)]
        a_n2 [(- i2 diffi) (- j2 diffj)]
        ] [a_n1 a_n2]))
(defn antinodes_for_antennaes [pairs] 
  (for [i (range (count pairs))
        j (range (inc i) (count pairs))]
    (antinodes_for_two_antennaes (nth pairs i) (nth pairs j))))
(defn in_bounds? [[i j] grid] 
  (and (>= i 0) (>= j 0) (< i (count grid)) (< j (count (first grid)))))
(antinodes_for_antennaes (first antennae_vec))
(def possible_antinodes (mapcat identity (mapcat antinodes_for_antennaes antennae_vec)))
possible_antinodes
(->> (filter #(in_bounds? % grid) possible_antinodes)
     (set)
     (count))
grid
;; Part 2
;; I have realized in the middle of this that i and j should be flipped
;; but we shall survive
(defn line_from_two [[i1 j1], [i2 j2]] 
  (let [slope (/ (- j2 j1) (- i2 i1))
        intercept (- j1 (* slope i1))
        ]
    [slope intercept]))
(defn lines_from_all [pairs] 
  (for [i (range (count pairs))
        j (range (inc i) (count pairs))]
    (line_from_two (nth pairs i) (nth pairs j))))
(defn is_point_on_line? [[slope intercept], [x, y]] 
  (= y (+ (* slope x) intercept)))
;; gonna use these to find if points are valid 
(def lines (map lines_from_all antennae_vec))
(def flatlines (mapcat identity lines))

(defn how_many_points [lines, grid] 
  (reduce 
   + 
   0 
   (for [i (range (count grid)) 
      j (range (count (first grid)))
      ] (let [ belongs? (some true? (map #(is_point_on_line? % [i j]) flatlines))]
          (if belongs? 1 0)))
   ))
(map #(is_point_on_line? % [0 11]) flatlines)
(how_many_points flatlines grid)
