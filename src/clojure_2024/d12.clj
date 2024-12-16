(require '[clojure.string :as str])

(defn process_input [input_string] 
  (let [split (str/split input_string #"\n")
        char_map (mapv vec split)
        ]
   char_map))

(def vis (atom #{}))
(defn visit? [[i j] prev_val grid]
  (let [v_bound? (< -1 i (count grid))
        h_bound? (< -1 j (count (first grid)))
        val_valid? (and v_bound? h_bound? (= prev_val (get-in grid [i j])))
        ] val_valid?))

(defn perimeter_and_to_visit_from_point [[i j] grid]
  (let [val (get-in grid [i j])
        up [(dec i) j]
        down [(inc i) j]
        left [i (dec j)]
        right [i (inc j)]
        would_visit (filter #(visit? % val grid) [up down left right])
        ;; Since if we're not visiting a point a fence needs to be built between
        ;; this point and that one
        perimeter (- 4 (count would_visit))
        to_visit (remove #(contains? @vis %) would_visit)
        _ (swap! vis clojure.set/union (set to_visit))] 
    [to_visit perimeter]))

(defn get_value_of_region [grid [i j]]
  (loop 
   [q (conj clojure.lang.PersistentQueue/EMPTY [i j]) perimeter_acc 0 area_acc 0 grid grid]
    (let [cur (peek q)
          _ (swap! vis clojure.set/union (set [cur]))
          popped_q (pop q)
          new_area (inc area_acc)
          [to_visit for_perimeter] (perimeter_and_to_visit_from_point cur grid)
          new_perimeter (+ perimeter_acc for_perimeter)
          new_q (apply conj popped_q to_visit)]
      (if (empty? new_q) (* new_area new_perimeter) (recur new_q new_perimeter new_area grid)))
    ))

(print vis)
(def vis (atom #{}))
(def raw-input (slurp "d12.txt"))
(def grid (process_input raw-input))
(defn get_all_regions [grid] 
  (for [i (range (count grid))
        j (range (count (first grid)))
        :when (not (contains? @vis [i j])) 
        ] (get_value_of_region grid [i j])
   ))
(def all_regions (get_all_regions grid))
(get_value_of_region grid [0 0])
(< 0 1 2)
(swap! vis clojure.set/union (set [[1,2]]))
(contains? @vis [1,2])
grid
(get-in grid [0 1])
