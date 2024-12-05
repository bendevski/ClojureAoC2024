(use 'clojure.java.io)
(require '[clojure.string :as str])

(with-open [rdr (reader "d1.txt")]
  (let [lines (line-seq rdr),
        str_pairs (map (fn [line] (str/split line #"   ")) lines)
        str_firsts (map first str_pairs)
        str_seconds (map last str_pairs)
        firsts (map parseInt str_firsts)
        seconds (map parseInt str_seconds)]
    (lines)))