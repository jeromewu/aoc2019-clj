(ns d13
  (:require [d05]))

(defn p1 [filename]
  (->>
    (d05/ic-com (hash-map :ptr 0 :in '() :out '() :base 0 :prog (d05/read-input filename)))
    (:out)
    (partition 3)))

(defn main [args]
  (p1 "data/d13-input.txt"))
