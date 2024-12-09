(require '[clojure.string :as str])
(def input_string (slurp "d5f.txt"))
(require 'sc.api)

(defn add_or_create_list_value_for_map [acc [v k]] (update acc k #(conj (or % []) v)))
(defn parse_input [input_str] 
  (let [[rules, books] (str/split input_str #"\n\n")
        rule_arr (str/split rules #"\n")
        rule_map (reduce add_or_create_list_value_for_map {} (map #(str/split % #"\|") rule_arr))
        books_arr (map #(str/split % #",") (str/split books #"\n"))
        ]
   [rule_map, books_arr]) )


(def rules_and_maps (parse_input input_string))
(def rules (first rules_and_maps))
(def sections (last rules_and_maps))
;; taken from https://stackoverflow.com/questions/3249334/test-whether-a-list-contains-a-specific-value-in-clojure\
(defn in? [coll elm] (some #(= elm %) coll))
(defn section_ready? [section, rules] 
  (loop [section section rules rules] 
    (let [cur (first section)
          following (rest section)
          mistakes (map #(in? following %) (get rules cur))
          end? (empty? following) 
          stop? (some true? (sc.api/spy mistakes))]
      (if end? true 
        (if stop? 
          false 
          (recur following rules)))
      )))
(defn value_of_section [section, rules] (if (section_ready? section, rules) (Integer/parseInt (get section (quot (count section) 2))) 0))
(defn solve_1 [sections, rules] (reduce + 0 (map #(value_of_section % rules) sections)))
(solve_1 sections rules)
;; section 2
;; taken from https://stackoverflow.com/questions/5979538/what-is-the-idiomatic-way-to-swap-two-elements-in-a-vector
(defn swap [v i1 i2] 
  (assoc v i2 (v i1) i1 (v i2)))
(defn find_mistake_index [rules v i len] 
  (loop [idx i length len]
    (let [mistake? (in? rules (get v idx))
          end? (= idx length)
          ] (if mistake? 
              idx
              (if end? 
                -1
                (recur (+ idx 1) length))))))
(ready_section problem_section rules)
(defn ready_section [section, rules]
  (let [len (count section)
        final_vec (loop [v section idx 0]
                    (let [end? (= idx (- len 1))
                          cur_rules (get rules (get v idx))
                          mistake_index (find_mistake_index cur_rules v idx len)
                          next_vec (if (= mistake_index -1) nil (swap v idx mistake_index))]
                      (if end? 
                        v
                        (if (nil? next_vec) (recur v (+ idx 1)) (recur next_vec idx))
                        )))] final_vec))

(def counted (remove #(section_ready? % rules) sections))
(reduce + 0 (map #(value_of_section % rules) (map #(ready_section % rules) counted)))