(ns d10)

(defn row-to-cells [row]
  (->> (seq (row :data))
       (map-indexed #(hash-map :x %1 :y (row :y) :data %2))))

(defn read-input [filename]
  (->> 
      (slurp filename)
      (re-seq #"[.|#]+")
      (map-indexed #(hash-map :y %1 :data %2))
      (reduce #(concat %1 (row-to-cells %2)) '())))

(defn round [pcs v]
  (->>
      (* v (Math/pow 10 pcs))
      (Math/round)
      (#(/ % (Math/pow 10 pcs)))))

(defn get-len [v]
  (Math/pow (+ (Math/pow (v :x) 2) (Math/pow (v :y) 2)) 0.5))

(defn dot [v1 v2]
  (+ (* (v1 :x) (v2 :x)) (* (v1 :y) (v2 :y))))

(defn get-angle [v]
  (->>
      (/ (dot {:x 0 :y -1} v) (get-len v))
      (Math/acos)
      (#(/ % Math/PI))
      (* 180)
      (#(if (neg? (v :x)) (- 360 %) %))
      (round 8)))

(defn get-len-and-angle [c1 c2]
  (let [v {:x (- (c2 :x) (c1 :x)) :y (- (c2 :y) (c1 :y))}]
    (hash-map :len (get-len v) :angle (get-angle v) :x (c2 :x) :y (c2 :y))))

(defn get-num-visibles [c cells]
  (->>
      (map #(get-len-and-angle c %) cells)
      (group-by #(% :angle))
      (into (sorted-map))
      (count)))

(defn get-destroy-order [c cells]
  (->>
      (map #(get-len-and-angle c %) cells)
      (group-by #(% :angle))
      (into (sorted-map))
      (vals)
      (map #(sort-by (fn [v] (v :len)) %))
      (#(reduce
          (fn [acc idx]
            (concat acc (map (fn [c] (nth c idx nil)) %)))
          '()
          (range (reduce max (map count %)))))
      (filter #(not= nil %))))

(defn p1 [filename]
  (let [cells (read-input filename)
        asteroids (filter #(= \# (% :data)) cells)]
    (->>
        (map #(assoc % :visibles (get-num-visibles % (filter (fn [c] (not= c %)) asteroids))) asteroids)
        (reduce #(if (> (%1 :visibles) (%2 :visibles)) %1 %2)))))

(defn p2 [filename x y target]
  (let [cells (read-input filename)
        asteroids (filter #(= \# (% :data)) cells)]
    (->>
        (filter #(and (= x (% :x)) (= y (% :y))) asteroids)
        (first)
        (#(get-destroy-order % (filter (fn [c] (not= c %)) asteroids)))
        (#(nth % (- target 1))))))

; (p1 "data/d10-input0.txt")
(p1 "data/d10-input.txt") ; (30, 34) 344
(p2 "data/d10-input.txt" 30 34 200)
