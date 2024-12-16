(require '[clojure.string :as str])
(defn process_input [raw_input] 
  (vec (str/split raw_input #" ")))
(defn fix_padding [num_str] (str (Long/parseLong num_str)))
(defn string_to_half_half_vec [num_str] 
  (let [mid_point (quot (count num_str) 2)
        valid? (not= mid_point 0)
        first_half (and valid? (fix_padding (subs num_str 0 mid_point)))
        second_half (and valid? (fix_padding (subs num_str mid_point (count num_str))))
        ] [first_half second_half]
    ))
(def num_state_map (atom {}))
(defn get_next_number_state [num_str]
  (let [solved? (contains? @num_state_map num_str)
        val (if solved? (get @num_state_map num_str)
                (if
                 (= num_str "0") "1"
                 (if (= (rem (count num_str) 2) 0) (string_to_half_half_vec num_str)
                     (fix_padding (str (* 2024 (Long/parseLong num_str)))))))
        _ (if (not solved?) (swap! num_state_map  assoc num_str val) nil)
        ]
    val))

(defn get_next_state [string_vec] 
  (flatten (pmap get_next_number_state  string_vec)))
(defn get_nth_step [input_string number]
  (loop [input_string input_string num 0]
    (if (= number num) input_string (recur (get_next_state input_string) (inc num))
     )))
(def raw-input (slurp "d11f.txt"))
(def final_input (process_input raw-input))
final_input
(count (get_nth_step final_input 25))
;; part 2
;; let's try this
(defn get_value_at_step [num_str steps]
  (let [solved? (contains? @num_state_map [num_str steps])
        val (cond
              solved? (get @num_state_map [num_str steps])
              (= steps 0) 1
              (= num_str "0") (get_value_at_step "1" (dec steps))
              (= (rem (count num_str) 2) 0)
              (let [[first_half second_half] (string_to_half_half_vec num_str) 
                    ](+ (get_value_at_step first_half (dec steps)) (get_value_at_step second_half (dec steps))))
              :else (get_value_at_step (fix_padding (str (* 2024 (Long/parseLong num_str)))) (dec steps)))
        _ (if (not solved?) (swap! num_state_map  assoc [num_str steps] val) nil)
        ]
    val))
(get_value_at_step "0" 4)
(reduce + (map #(get_value_at_step % 75) final_input))