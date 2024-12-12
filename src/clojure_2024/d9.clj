(require '[clojure.string :as str])

(defn expand_possible_string_pair [possibe_pair]
  (let [split (str/split possibe_pair #"")
        el1 (first split)
        el2 (first (rest split))
        ;; Use z cause it'll be another transformation to find each id
        expanded1 (repeat (Integer/parseInt el1) "z")
        expanded2 (if (nil? el2) [] (repeat (Integer/parseInt el2) "."))] (concat expanded1 expanded2)))
(defn process_input [input_string]
  (let [pairs (re-seq #".{1,2}" input_string)
        memory_blocks (map expand_possible_string_pair pairs)
        memory_blocks_with_id (map-indexed (fn [idx mem] (map #(if (= % "z") (str idx) %) mem)) memory_blocks)]
    (vec (flatten memory_blocks_with_id))))

(defn swap [v i1 i2]
  (assoc v i2 (v i1) i1 (v i2)))

(defn swap_memory [front_idx, back_idx, memory]
  (loop [front_idx front_idx back_idx back_idx memory memory] (let [front_el (get memory front_idx)
                                                                    back_el (get memory back_idx)
                                                                    change_front? (not= front_el ".")
                                                                    change_back? (= back_el ".")
                                                                    dont_swap? (or change_front? change_back?)
                                                                    updated_memory (if dont_swap? memory (swap memory front_idx back_idx))
                                                                    update_front (or (not dont_swap?) change_front?)
                                                                    update_back (or (not dont_swap?) change_back?)
                                                                    updated_front_idx (if update_front (inc front_idx) front_idx)
                                                                    updated_back_idx (if update_back (dec back_idx) back_idx)] (if (= front_idx back_idx) memory (recur updated_front_idx updated_back_idx updated_memory)))))

(defn get_swaped_mem_checksum [swaped_mem]
  (reduce-kv
   (fn [acc k v] (+ acc (* k (if (not= "." v) (Integer/parseInt v) 0))))
   0
   swaped_mem))
(def input_string (slurp "d9f.txt"))
(def memory (process_input input_string))
(def swaped_mem (swap_memory 0 (dec (count memory)) memory))
swaped_mem
(get_swaped_mem_checksum swaped_mem)
;; Part 2
(defn expand_possible_string_pair [possibe_pair]
  (let [split (str/split possibe_pair #"")
        el1 (first split)
        el2 (first (rest split))
        ;; Use z cause it'll be another transformation to find each id
        expanded1 (vec (repeat (Integer/parseInt el1) "z"))
        expanded2 (if (nil? el2) [] (vec (repeat (Integer/parseInt el2) ".")))] (if (empty? expanded2) [expanded1] [expanded1 expanded2])))
(defn give_indexes [grid]
  (loop [mem_idx 0 i 0 grid grid]
    (let [cur_block (get grid i)
          _ (print cur_block "\n")
          is_full? (not= (first cur_block) ".")
          new_mem_idx (if is_full? (inc mem_idx) mem_idx)
          new_block (if is_full? (repeat (count cur_block) (str mem_idx)) cur_block)
          new_grid (if is_full? (assoc grid i new_block) grid)]
      (if (= i (dec (count grid))) new_grid (recur new_mem_idx (inc i) new_grid)))))
(defn process_input [input_string]
  (let [pairs (re-seq #".{1,2}" input_string)
        memory_blocks (map expand_possible_string_pair pairs)
        formatted_memory_blocks (vec (mapcat identity memory_blocks))
        _ (print formatted_memory_blocks)
        memory_blocks_with_id (give_indexes formatted_memory_blocks)]

    memory_blocks_with_id))
(defn swap [v i1 i2]
  (assoc v i2 (v i1) i1 (v i2)))
(defn insert_free_space [v idx len]
  (vec (concat (subvec v 0 idx) [(repeat len ".")] (subvec v idx))))
(defn pretty-print-2d [grid]
  (doseq [row grid]
    (println (apply str row)))) ; Concatenate characters in a row and print

(defn remove-at [v idx]
  (vec (concat (subvec v 0 idx) (subvec v (inc idx)))))

(defn combine_free [grid]
  (loop [i 0 grid grid]
    (let [done? (>= i (- (count grid) 2))
          back (and (not done?) (get grid i))
          front (and (not done?) (get grid (inc i)))
          ;; Can only happen if they're dots
          process? (and (not done?) (= (first front) (first back)))
          new_front (if process? (concat back front) front)
          new_grid (if process? (assoc (remove-at grid i) i new_front) grid)
          new_i (if process? i (inc i))] 
      (if (not done?) (recur new_i new_grid) new_grid))))

(defn swap_and_add_mem [grid front_idx back_idx]
  (let [front_el (get grid front_idx)
        back_el (get grid back_idx)
        size_diff (- (count front_el) (count back_el))
        needs_padding? (> size_diff 0)
        ;; done so that when we try to switch [...] [22]
        ;; we don't get [22] [.] [...] but [22][.] [..]
        updated_free_space (repeat (count back_el) ".")
        balanced_grid (assoc grid front_idx updated_free_space)
        swapped (swap balanced_grid front_idx back_idx)
        padded (if needs_padding? (insert_free_space swapped (inc front_idx) size_diff) swapped)] padded))
(defn find_space [back_idx grid]
  (loop [front_idx 0 back_idx back_idx grid grid]
    (let [front_el (get grid front_idx)
          back_el (get grid back_idx)
          free? (= (first front_el) ".")
          big_enough? (<= 0 (- (count front_el) (count back_el)))]
      (if (= front_idx back_idx) nil (if (and free? big_enough?) front_idx (recur (inc front_idx) back_idx grid))))))
(defn next_grid_state [grid, moved_map]
  (loop [grid grid back_idx (dec (count grid)) moved_map moved_map]
    (let [back_el (get grid back_idx)
          valid_back? (not= (first back_el) ".")
          front_idx (if valid_back? (find_space back_idx grid) nil)
          cant_move? (or (get moved_map (first back_el)) (nil? front_idx))
          new_moved_map (if cant_move? moved_map (assoc moved_map (first back_el) true))]
      (if
       (= back_idx 0)
        nil
        (if cant_move?
          (recur grid (dec back_idx) new_moved_map)
          [(swap_and_add_mem grid front_idx back_idx)])))))
(defn move_memory [grid moved_map]
  (loop [grid grid moved_map moved_map]
    (let [[new_grid, new_moved_map] (next_grid_state grid moved_map)
          _ (if (not (nil? new_grid)) (do (pretty-print-2d new_grid) (print "\n")) nil)]
      (if (nil? new_grid) grid (recur new_grid new_moved_map)))))
(< 1 0)
;; swap if first is dots second is not dots and the size fits
;; change first if it's not dots or doesn't fit
;; change back if swap or is dots
;;
(>= 1 0)
(- 3 2)
(def input_string (slurp "d9f.txt"))
;; Too lazy to make this the right size
(def mem (process_input input_string))
mem
(def moved_map (zipmap (map str (range (count mem))) (repeat false)))

mem
(move_memory mem moved_map)
(next_grid_state mem)
(swap_memory 0 (- (count mem) 1) mem)
(- 10 9)
(>= 1 0)