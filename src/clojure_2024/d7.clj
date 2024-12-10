(require '[clojure.string :as str])
(require 'sc.api)

(def input_string (slurp "d7f.txt"))
(defn parse_input [input_str] 
  (let [lines (str/split input_str #"\n")
       [str_res, str_ops] (apply mapv vector (map #(str/split % #": ") lines))
        res (map #(Long/parseLong %) str_res)
        str_op_arr (map #(str/split % #" ") str_ops)
        ops (map (fn [str_op_arr](map #(Long/parseLong %) str_op_arr)) str_op_arr)
        ]
   [res, ops]))
(defn pair_possible? [res, cur, ops] 
    (let [possible? (and (empty? ops) (or (= cur res) (= res nil)))
          not_possible? (or (> cur res) (empty? ops))
          ]
      
      (if possible?
        true
        (if not_possible?
          false
          (or
           (pair_possible? res (* cur (first ops)) (rest ops))
           (pair_possible? res (+ cur (first ops)) (rest ops)))))))
(def parsed (parse_input input_string))
(def ress (first parsed))
(def opss (last parsed))
(def first_res (first ress))
(def first_ops (first opss))
(defn solve1 [ress, opss] 
  (loop [acc 0 ress ress opss opss]
    (let [cur_res (first ress)
          cur_ops (first opss)
          next_ress (rest ress)
          next_opss (rest opss)
          cur_val (if (pair_possible? cur_res (first cur_ops) (rest cur_ops)) cur_res 0)
          cur_acc (+ acc cur_val)]
      (if (empty? next_ress) 
        cur_acc
        (recur cur_acc next_ress next_opss)
        )
      ) 
    ))
(pair_possible? first_res (first first_ops) (rest first_ops))
(sort > (map #(count %) opss))
(solve1 ress opss)
;; part 2
;; Just realized I overcomplicated it...
(defn get_new_res_after_pipe [res, cur] 
  (let [res_string (str res)
        cur_string (str cur)
        valid? (str/starts-with? res_string cur_string)
        new_res_string (if valid? (str/replace-first res_string cur_string "") nil)
        new_res (if valid? (if (not= new_res_string "") (Long/parseLong new_res_string) nil) nil)
        ]
    new_res))
(defn pair_possible? [res, cur, ops] 
    (let [possible? (and (empty? ops) (or (= cur res) (= res nil)))
          not_possible? (or (> cur res) (empty? ops))
          pipe (Long/parseLong (str (str cur) (str (first ops))))
          _ (print res cur ops pipe "\n")
          ]
      (if possible?
        true
        (if not_possible?
          false
          (or
           (pair_possible? res (* cur (first ops)) (rest ops))
           (pair_possible? res (+ cur (first ops)) (rest ops))
           (pair_possible? res pipe (rest ops)))))))


(defn solve2 [ress, opss] 
  (loop [acc 0 ress ress opss opss]
    (let [cur_res (first ress)
          cur_ops (first opss)
          next_ress (rest ress)
          next_opss (rest opss)
          possible? (pair_possible? cur_res (first cur_ops) (rest cur_ops))
          cur_val (if possible? cur_res 0)
          cur_acc (+ acc cur_val)]
      (if (empty? next_ress) 
        cur_acc
        (recur cur_acc next_ress next_opss)
        )
      ) 
    ))

(solve2 ress opss)