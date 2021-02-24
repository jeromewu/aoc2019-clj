(ns d06)

(defn read-input [filename]
  (->> (slurp filename)
       (re-seq #"[\w|)]+")
       (map #(re-seq #"\w+" %))))

(defn get-obj-set [pairs]
  (->> (reduce #(concat %1 %2) '() pairs)
       (set)))

(defn get-c2p-map [pairs]
  (reduce
    #(assoc %1 (second %2) (first %2))
    {}
    pairs))

(def get-num-orbits
  (memoize
    (fn [c2p-map obj]
      (if (= "COM" obj)
        0
        (->> (c2p-map obj)
             (get-num-orbits c2p-map)
             (+ 1))))))

(defn get-path [c2p-map obj]
  (let [parent-obj (c2p-map obj)]
    (if (= "COM" obj)
      (list obj)
      (concat (list obj) (get-path c2p-map parent-obj)))))

(defn get-path-its [c2p-map]
  (let [you-path-set (set (get-path c2p-map "YOU"))
        san-path-set (set (get-path c2p-map "SAN"))]
    (clojure.set/intersection you-path-set san-path-set)))

(defn p1 [filename]
  (let [orbits-map (read-input filename)
        obj-set (get-obj-set orbits-map)
        c2p-map (get-c2p-map orbits-map)]
    (->> (map #(get-num-orbits c2p-map %) obj-set)
         (reduce +))))

(defn p2 [filename]
  (let [c2p-map (get-c2p-map (read-input filename))
        path-its (get-path-its c2p-map)
        num-orbits-its (->> (map #(get-num-orbits c2p-map %) path-its) (reduce max))
        num-orbits-you (get-num-orbits c2p-map "YOU")
        num-orbits-san (get-num-orbits c2p-map "SAN")]
    (->> (+ num-orbits-you num-orbits-san)
         (+ (* 2 (- num-orbits-its)))
         (+ (- 2)))))

(p1 "data/d06-input0.txt")
(p1 "data/d06-input.txt")
(p2 "data/d06-input0.txt")
(p2 "data/d06-input.txt")
