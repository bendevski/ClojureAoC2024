(require '[clojure.string :as str])
(def input_string (slurp "d4f.txt"))

(def grid (str/split input_string #"\n" ))
(defn transpose [grid] (map str/join (apply map vector grid)))
(transpose grid)
(defn diagonals [grid] 
(let [
      rows (count grid)
      cols (count (first grid))
      idxs (for [i (range rows) j (range cols)] [i j])]
      (concat (vals (group-by (fn [[i j]] (+ i j)) idxs)) (vals (group-by (fn [[i j]] (- i j)) idxs)))))
(defn count_XMAS [entry] 
  (->> (partition 4 1 entry)
       (map #(apply str %))
       (filter #{"XMAS" "SAMX"})
       count))
(defn get_value_from_idx_pair [idx_pair grid] (get-in grid idx_pair))
(defn get_values_from_idx_pairs [idx_pairs grid] (map #(get_value_from_idx_pair % grid) idx_pairs))
(defn get_diag_values [pairs, grid] (map #(get_values_from_idx_pairs % grid) pairs))
(defn get_string_diags [grid] (map str/join (get_diag_values (diagonals grid) grid)))
(def all_options (concat grid (transpose grid) (get_string_diags grid)))
all_options
grid
(let [diagonals_v (diagonals grid)
      vals (get_diag_values  diagonals_v grid)] vals)

(get_values_from_idx_pairs [[0 0]] grid)
(get_string_diags grid)
(reduce + 0 (map count_XMAS all_options))
(count_XMAS input_string)
(defn not_edge [i j grid] 
  (and (not= (- i 1) (count grid)) (not= i 0) (not= j 0) (not= (- j 1) (count (first grid)))))

;; Part 2
;; find all As
(defn all_a [grid] 
  (let [rows (count grid)
        cols (count (first grid))
        pairs (for [i (range rows) 
                    j (range cols)
                    :when (and (= (get-in grid [i j]) \A) (not_edge i j grid))] [i, j])]
        pairs))

(defn get_cross_chars [loc grid] 
  (let [i (first loc)
        j (last loc)
        loc_char (get-in grid [i j])
        top_left (get-in grid [(- i 1) (- j 1)])
        top_right (get-in grid [(- i 1) (+ j 1)])
        bottom_left (get-in grid [(+ i 1) (- j 1)])
        bottom_right (get-in grid [(+ i 1) (+ j 1)])
        all_chars [[top_left loc_char bottom_right] [top_right loc_char bottom_left]]
        ]all_chars))


(def all_as (all_a grid))
(def crosses (map #(get_cross_chars % grid) all_as))
(defn get_cross_string [cross] (map str/join cross))
(defn get_cross_strings [crosses] (map get_cross_string crosses))
(defn string_valid? [string] (or (= string "MAS") (= string "SAM")))
(defn cross_valid? [cross] 
  (let [d1 (first cross)
        d2 (last cross)
        valid (and (string_valid? d1) (string_valid? d2))
        ] valid))
(defn are_crosses_valid [cross_strings] (map cross_valid? cross_strings))

(->> crosses
     (get_cross_strings)
     (are_crosses_valid)
     (filter true?)
     (count))
(->> crosses
     (get_cross_strings))