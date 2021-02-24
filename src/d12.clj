(ns d12)

(defn abs [n]
  (max n (- n)))

(defn read-input [filename]
  (->> (slurp filename)
       (re-seq #"[\d|-]+")
       (map #(Integer/parseInt %))
       (partition 3)))

(defn get-abs-sum [col]
      (reduce #(+ %1 (abs %2)) 0 col))

(defn get-total-energy [poss vels]
  (let [
        pots (map #(get-abs-sum %) poss)
        kins (map #(get-abs-sum %) vels)]
    (->>
      (map * pots kins)
      (reduce +))))

(defn get-vel-delta [pos poss]
  (map-indexed 
    (fn [i v0] 
      (reduce
        #(let [v1 (nth %2 i)]
          (+ %1 (cond (> v0 v1) -1 (= v0 v1) 0 (< v0 v1) 1)))
        0
        poss))
    pos))

; (->>
;   (read-input "data/d12-input0.txt")
;   (#(get-vel-delta (nth % 3) %)))

(defn update-vels [poss vels]
  (map-indexed
    #(->>
       (get-vel-delta %2 poss)
       (map + (nth vels %1)))
    poss))

(defn update-poss [poss vels]
  (map-indexed
    #(map + %2 (nth vels %1))
    poss))

(defn simulate [poss vels step num-steps]
  (prn (get-total-energy poss vels))
  (let [
        next-vels (update-vels poss vels)
        next-poss (update-poss poss next-vels)]
        ; _ (prn poss)]
    (if (= step num-steps)
      (get-total-energy poss vels)
      (recur
        next-poss
        next-vels
        (inc step)
        num-steps))))

(defn p1 [filename]
  (simulate
    (read-input filename)
    (partition 3 (repeat 12 0))
    0
    100))

(defn get-next-v [ps vs]
  (->>
    (map 
      (fn [p]
        (reduce #(+ %1 (cond (> p %2) -1 (= p %2) 0 (< p %2) 1)) 0 ps)) ps)
    (map + vs)))

(defn get-num-steps [ps0 vs0 ps vs step]
  (let [
        nest-vs (get-next-v ps vs)
        nest-ps (map + ps nest-vs)]
    (if (or (and (= ps0 ps) (= vs0 vs) (not= step 0)) (= step -1))
      step
      (recur ps0 vs0 nest-ps nest-vs (inc step)))))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn p2 [filename]
  (let [
        poss (read-input filename)
        v0 (repeat 4 0)]
    (->>
      (range 3)
      (map (fn [idx] (map #(nth % idx) poss)))
      (map #(get-num-steps % v0 % v0 0))
      (reduce #(lcm %1 %2)))))

(defn main [args]
  (p2 "data/d12-input.txt"))
