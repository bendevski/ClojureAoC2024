(use 'clojure.java.io)
(require '[clojure.string :as str])
(require 'sc.api)

(defn get_direction [seq] (< (first seq) (second seq)))
(defn valid_magnitude [seq] (<= 1 (abs (- (first seq) (second seq))) 3))

(defn is_sequence_safe [seq]
  (let [direction (get_direction seq),
        safety (loop [inside_seq seq]
                 (if (empty? (rest inside_seq)) true
                     (if (and (= (get_direction inside_seq) direction) (valid_magnitude inside_seq))
                       (recur (rest inside_seq))
                       false)))] safety))

(with-open [rdr (reader "d2f.txt")]
  (let [lines (doall (line-seq rdr)),
        str_seqs (map #(str/split % #" ") lines),
        seqs (map (fn [str_seq] (map #(Integer/parseInt %) str_seq)) str_seqs),
        true_false (map is_sequence_safe seqs),
        final (get (frequencies true_false) true 0)] final)
  )
  
(defn parse_string_seq [string_seq] (map #(Integer/parseInt %) string_seq))

(defn get_permutations [lst]
  (map-indexed (fn [idx, _] (concat (take idx lst) (drop (inc idx) lst))) lst))

(def q2_input (with-open [rdr (reader "d2f.txt")]
                (let [lines (doall (line-seq rdr)),
                      str_seqs (map #(str/split % #" ") lines),
                      seqs (map parse_string_seq str_seqs),
                      permutations (map get_permutations seqs),
                      true_false (map (fn [perms] (map is_sequence_safe perms)) permutations),
                      true_false_reduces (map #(some true? %)  true_false),
                      trues (get (frequencies true_false_reduces) true 0)
                      ]trues)))
q2_input

(defn is_sequence_safe_2 [seq]
  (let [direction (get_direction seq),
        safety (loop [inside_seq seq changes 0]
                 (if (empty? (rest inside_seq)) true
                     (if (and (= (get_direction inside_seq) direction) (valid_magnitude inside_seq))
                       (recur (rest inside_seq) changes)
                       ()
                       false)))] safety))
