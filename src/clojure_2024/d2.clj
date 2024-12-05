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
  
(with-open [rdr (reader "d2.txt")]
  (let [lines (doall (line-seq rdr)),
        str_seqs (map #(str/split % #" ") lines),
        ] str_seqs)
  ) 