(ns d09
  (:require [d05]))

(defn p1 [filename in]
  (->> (d05/ic-com (hash-map :ptr 0 :in in :out '() :base 0 :prog (d05/read-input filename)))
    (:out)
    (last)))

(defn main [args]
  (->>
    (p1 "data/d09-input.txt" '("2"))
    (prn)))
