(require '[clojure.string :as str])
(require 'sc.api)

(def input_string (slurp "d6f.txt"))
(def grid (mapv vec(str/split input_string #"\n")))
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
(defn in_bounds? [[i j] grid] 
  (let [rows (count grid)
        cols (count (first grid))
        ibounds? (and (>= i 0) (< i rows) (>= j 0) (< j cols))
        ]
   ibounds?))
(def next_direction 
  {[1 0] [0 -1] 
   [0 -1] [-1 0] 
   [-1 0] [0 1] 
   [0 1] [1 0]})

(defn get_next_state [cur_loc direction grid]
  (let [cur_i (first cur_loc)
        cur_j (last cur_loc)
        next_state (loop [direction direction grid grid]
                     (let [delta_i (first direction)
                           delta_j (last direction)
                           natural_next [(+ cur_i delta_i) (+ cur_j delta_j)]
                           out_of_bounds? (not (in_bounds? natural_next grid))
                           wall? (and (not out_of_bounds?) (= (get-in grid natural_next) \#))]
                       (if out_of_bounds? 
                         nil 
                         (if wall?
                           (recur (next_direction direction) grid)
                           [natural_next, direction]))))]
    next_state)
  )
(defn walk [cur_loc direction grid] 
  (loop [loc cur_loc direction direction grid grid]
    (let [new_grid (assoc-in grid loc \W)
          next_state (get_next_state loc direction grid) 
          ](if (nil? next_state)
             new_grid
             (recur (first next_state) (last next_state) new_grid)))))
(def walked_grid (walk start [-1 0] grid))
(defn filter_Ws [lis] (filter #(= % \W) lis))
(defn count_walked [grid] (reduce + (map count (map filter_Ws grid))))
(count_walked walked_grid)
;; Part 2
(def direction_sign {[1 0] \D
                     [0 -1] \L
                     [-1 0] \U
                     [0 1] \R})
(defn is_inifinite_walk? [cur_loc direction base_grid] 
  (loop [loc cur_loc direction direction grid base_grid]
    (let [new_grid (assoc-in grid loc (direction_sign direction))
          next_state (get_next_state loc direction grid)
          infinite? (= (get-in grid (first next_state)) (direction_sign (last next_state)))
          ] (if infinite? true (if (nil? next_state)
             false
             (recur (first next_state) (last next_state) new_grid))))))
(defn walk [cur_loc original_direction base_grid] 
  (loop [loc cur_loc direction original_direction grid base_grid infinities 0 checked #{}]
    (let [new_grid (assoc-in grid loc (direction_sign direction))
          next_state (get_next_state loc direction grid)
          infinite_if_wall? (and (not (nil? next_state)) (not (contains? checked (first next_state))) (is_inifinite_walk? cur_loc original_direction (assoc-in base_grid (first next_state) \#)))
          local_infinite (if infinite_if_wall? (inc infinities) infinities)
          ](if (nil? next_state)
             [new_grid, local_infinite]
             (recur (first next_state) (last next_state) new_grid local_infinite (conj checked (first next_state)))))))

(def new_walked (walk start [-1 0] grid))
new_walked