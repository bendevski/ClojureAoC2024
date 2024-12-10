(use 'clojure.java.io)
(require '[clojure.string :as str])
;; part 1
(with-open [rdr (reader "d1.txt")]
  (let [lines (doall (line-seq rdr)),
        str_pairs (map (fn [line] (str/split line #"\s+")) lines),
        str_firsts (map first str_pairs),
        str_seconds (map last str_pairs),
        firsts (map #(Integer/parseInt %) str_firsts),
        seconds (map #(Integer/parseInt %) str_seconds),
        sorted_firsts (sort firsts),
        sorted_seconds (sort seconds),
        results (map - sorted_seconds sorted_firsts),
        abs_results (map abs results),
        final (reduce + abs_results)]
    final))
;; part 2
(with-open [rdr (reader "d1.txt")]
  (let [lines (doall (line-seq rdr)),
        str_pairs (map (fn [line] (str/split line #"\s+")) lines),
        str_firsts (map first str_pairs),
        str_seconds (map last str_pairs),
        firsts (map #(Integer/parseInt %) str_firsts),
        seconds (map #(Integer/parseInt %) str_seconds),
        freqs (frequencies seconds), 
        multiplied (map (fn [elem] (* elem (get freqs elem 0))) firsts),
        final (reduce + multiplied)]
    final))