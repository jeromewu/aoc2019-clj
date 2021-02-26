(ns d14)

(defn read-input [filename]
  (->>
    (slurp filename)
    (re-seq #".+")
    (map
      (fn [line]
        (->>
           (re-seq #"\w+"line)
           (partition 2)
           (map #(hash-map :qty (Integer/parseInt (first %)) :chem (second %))))))
    (vec)))

(def get-formula
  (memoize
    (fn [formulas chem]
      (reduce #(if (= chem (:chem (last %2))) (reduced %2) %1) nil formulas))))

(defn get-num-ores [formulas chems])

(defn p1 [filename]
  (get-num-ores
    (read-input filename)
    {:qty 1 :chem "FUEL"}))

(defn main [args]
  (->>
    (read-input "data/d14-input0.txt")
    (#(get-formula % "FUEL"))))
