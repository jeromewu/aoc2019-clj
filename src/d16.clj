(ns d16)

(def base [0 1 0 -1])

(defn read-input [filename]
  (->>
    (slurp filename)
    (re-seq #"\d")
    (map #(Integer/parseInt %))
    (vec)))

(defn rightmost [n]
  (mod (max n (- n)) 10))

(def get-pattern
  (memoize
    (fn [len n-repeat]
      (let [len+ (inc len)]
        (reduce
          #(let [pattern (concat %1 (repeat n-repeat (nth base (mod %2 (count base)))))]
             (if (>= (count pattern) len+)
               (reduced (drop 1 (take len+ pattern)))
               pattern))
          []
          (range len+))))))

(defn fft [phase n-phases digits]
  (prn phase)
  (let [len (count digits)]
    (if (= phase n-phases)
      digits
      (->>
        (map
          #(let [pattern (get-pattern len (inc %))]
            (->>
              (map * digits pattern)
              (reduce +)
              (rightmost)))
          (range len))
        (recur (inc phase) n-phases)))))

(defn p1 [filename n-phases]
  (let [digits (read-input filename)]
    (->>
      (fft 0 n-phases digits)
      (take 8)
      (reduce str))))

(defn p2-back [digits]
  (reductions #(mod (+ %1 %2) 10) digits))

(defn p2 [filename n-phases]
  (let [digits (read-input filename)
        digits-repeated (reduce into (repeat 10000 digits))
        offset (Integer/parseInt (reduce str (take 7 digits)))]
    (reduce str (take 8 (reverse (nth (iterate p2-back (reverse (drop offset digits-repeated))) n-phases))))))

(defn main [args]
  (prn (p1 "data/d16-input.txt" 100))
  (prn (p2 "data/d16-input.txt" 100)))
