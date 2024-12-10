(use 'clojure.java.io)
(require '[clojure.string :as str])
(require 'sc.api)

(def input_string (slurp "d3f.txt"))

(def valids (re-seq #"mul[\(]\d+,\d+[\)]" input_string))

(defn string_pairs [valids] (map #(re-seq #"\d+" %) valids))
(defn strings_to_ints [lis] (map #(Integer/parseInt %) lis))
(defn num_pairs [string_pairs] (map strings_to_ints string_pairs))
(defn calculate_final_number [num_pairs] (reduce + (map #(* (first %) (last %)) num_pairs)))
(->> valids
(string_pairs)
(num_pairs)
(calculate_final_number))


(def valids2 (re-seq #"mul[\(]\d+,\d+[\)]|do\(\)|don\'t\(\)" input_string))
(defn is_do_nt? [entry] (or (= entry "do()") (= entry "don't()")))
(def to_process? {"don't()" false "do()" true})
(defn process_entry [entry, include]
  (if (not include) 0 
      (->> entry
           (re-seq #"\d+")
           (strings_to_ints)
           (reduce *))))
(defn process_entries [entries] 
  (loop [entries entries include true acc 0]
    (if (empty? entries) acc 
    (if (is_do_nt? (first entries)) 
      (recur (rest entries) (to_process? (first entries)) acc)
      (recur (rest entries) include (+ acc (process_entry (first entries) include))))
      )))
(process_entries valids2)
(to_process? "don't()")
valids2