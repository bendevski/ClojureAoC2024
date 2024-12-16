(require '[clojure.string :as str] )
(def input_string (slurp "d10f.txt"))
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
  (loop [acc 0 queue (conj clojure.lang.PersistentQueue/EMPTY point) grid grid vis #{}]
    (let [cur (peek queue)
          popped (pop queue)
          visited? (contains? vis cur)
          done? (= (Integer/parseInt (get-in grid cur)) 9)
          process? (not (or visited? done?))
          next_acc (if (and (not visited?) done?) (inc acc) acc)
          new_vis (if (not visited?) (conj vis cur) vis)
          next_q (if process? (apply conj popped (valid_points cur grid)) popped)]
      (if (empty? next_q) next_acc (recur next_acc next_q grid new_vis)))))

(def starts (get_starts grid))
(available_paths grid (first starts))
(map #(available_paths grid %) starts)
(reduce + (map #(available_paths grid %) starts))
;; Part 2

(defn available_paths2 [grid point]
  (loop [acc 0 queue (conj clojure.lang.PersistentQueue/EMPTY point) grid grid]
    (let [cur (peek queue)
          popped (pop queue)
          done? (= (Integer/parseInt (get-in grid cur)) 9)
          next_acc (if done? (inc acc) acc)
          next_q (apply conj popped (valid_points cur grid))]
      (if (empty? next_q) next_acc (recur next_acc next_q grid)))))

(reduce + (map #(available_paths2 grid %) starts))

