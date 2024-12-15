(require '[clojure.string :as str] )
(def input_string (slurp "d10.txt"))
(defn process_input [unprocessed_input] 
  (let [lines (str/split unprocessed_input #"\n")
        processed (map #(str/split % #"") lines)
        ]
   (vec processed)))
(def grid (process_input input_string))
(defn get_starts [grid]
  (for [i (range (count grid))
        j (range (count (first grid)))
        :when (= (get-in grid [i j]) "0")]
    [i j]))
(get_starts grid)
(defn valid_point [[i j] prev_value grid]
  (let [vertical_bounds? (and (< i (count grid)) (>= i 0))
        horizontal_bounds? (and (< j (count (first grid))) (>= j 0))
        cur_value (and horizontal_bounds? vertical_bounds? (get-in grid [i j]))
        value_correct? (and cur_value (= (Integer/parseInt prev_value) (dec (Integer/parseInt cur_value))))
        ]
    value_correct?))
(defn valid_points [[i j] grid]
  (let [cur_value (get-in grid [i j])
        left [i (dec j)]
        right [i (inc j)]
        up [(dec i) j]
        down [(inc i) j] 
        output (filter #(valid_point % cur_value grid) [left, right, up, down])]
   (vec output)))
(defn available_paths [grid point]
  (loop [queue (conj clojure.lang.PersistentQueue/EMPTY point) grid grid vis {}]
    (let [cur (peek queue)
          next_q (pop queue)
          
          ])
    ))
(def test_grid [["1"] ["2"] ["3"]])
(valid_points [0,0] test_grid)
(def q (conj clojure.lang.PersistentQueue/EMPTY 0))
(pop q)
(first q)
(count (first test_grid))
(>= 1 0)
