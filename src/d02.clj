(ns d02)

(defn assoc-col [index v col]
  (map-indexed #(if (= index %1) (str v) %2) col))

(defn read-input [filename]
  (->>
    (slurp filename)
    (re-seq #"\d+")))

(defn update-ptr [ptr prog]
  (let [opcode (nth prog ptr)]
    (cond
      (= opcode "1") (+ 4 ptr)
      (= opcode "2") (+ 4 ptr))))

(defn update-prog [ptr prog]
  (let [
        opcode (nth prog ptr)
        v1-index (Integer/parseInt (nth prog (+ 1 ptr)))
        v2-index (Integer/parseInt (nth prog (+ 2 ptr)))
        index (Integer/parseInt (nth prog (+ 3 ptr)))
        v1 (Integer/parseInt (nth prog v1-index))
        v2 (Integer/parseInt (nth prog v2-index))]
    (cond
      (= opcode "1") (assoc-col index (+ v1 v2) prog)
      (= opcode "2") (assoc-col index (* v1 v2) prog)
      :else '())))

(defn run-prog [ctx]
  (let [prog (ctx :prog)
        ptr (ctx :ptr)
        opcode (nth prog ptr)]
    (if (= "99" opcode)
      prog
      (run-prog (hash-map :ptr (update-ptr ptr prog) :prog (update-prog ptr prog))))))

(defn p1 [filename]
  (->>
    (read-input filename)
    (hash-map :ptr 0 :prog)
    (run-prog)
    (first)))

(defn p2 [filename target]
  (let [prog (read-input filename)]
    (->>
      (for [n (range 100) v (range 100)] (hash-map :n n :v v))
      (reduce
        (fn [ans pair]
          (->>
           (assoc-col 1 (pair :n) prog)
           (assoc-col 2 (pair :v))
           (hash-map :ptr 0 :prog)
           (run-prog)
           (#(if (= target (first %)) (reduced pair) ans))))
        nil)
      (#(+ (% :v) (* 100 (% :n)))))))

(p1 "data/d02-input.txt")
(p2 "data/d02-input.txt" "19690720")
