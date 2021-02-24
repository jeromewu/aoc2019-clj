(ns d08)

(defn read-input [filename]
  (->> (slurp filename)
       (re-seq #"\d")))

(defn count-digits [digit col]
  (count (filter #(= digit %) col)))

(defn get-layers [width height pixels]
  (->>
    (partition (* width height) pixels)
    (map #(hash-map :pixels % :zeros (count-digits "0" %)))))

(defn p1 [filename width height]
  (let [layers (->> (read-input filename) (get-layers width height))]
    (->> (reduce #(if (< (%1 :zeros) (%2 :zeros)) %1 %2) layers)
         (#(get %2 %1) :pixels)
         (#(* (count-digits "1" %) (count-digits "2" %))))))

(defn get-color [digits]
  (->> (seq digits)
       (reduce #(if (not= \2 %2) (reduced %2) \2) \2)
       (str)))

(defn p2 [filename width height]
  (->> (read-input filename)
       (partition (* width height))
       (reduce #(map str %1 %2))
       (map get-color)
       (reduce str)
       (seq)
       (partition width)
       (map #(reduce str %))
       (map prn)))

(p1 "data/d08-input.txt" 25 6)
(p2 "data/d08-input.txt" 25 6)
