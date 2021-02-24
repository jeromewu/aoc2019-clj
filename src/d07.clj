(ns d07
  (:require [clojure.math.combinatorics :as combo]
            [d05]))

(defn halt? [ctx]
  (= '(\9 \9) (take-last 2 (nth (:prog ctx) (:ptr ctx)))))

(halt? (hash-map :ptr 0 :prog '("99")))

(defn p1 [filename]
  (->>
    (combo/permutations '("0" "1" "2" "3" "4"))
    (map 
      (fn [pseq]
        (reduce
          #(->>
             (hash-map :ptr 0 :in (list %2 %1) :out '() :prog (d05/read-input filename))
             (d05/ic-com)
             (:out)
             (last))
          "0"
          pseq)))
    (reduce max)))

(p1 "data/d07-input0.txt")

(defn fb-loop [loop-in ctxs]
  (if (halt? (last ctxs))
    loop-in
    (->>
      (reductions
        (fn [prev-ctx n]
          (let [
                in (last (:out prev-ctx))
                ctxn (nth ctxs n)
                ctx (assoc ctxn :in (concat (:in ctxn) (list (str in))))]
            (d05/ic-com ctx)))
        (hash-map :out (list loop-in))
        (range (count ctxs)))
      (drop 1)
      (#(fb-loop (last (:out (last %))) %)))))

(defn p2 [filename]
  (let [
        phase-seqs (combo/permutations '("5" "6" "7" "8" "9"))
        prog (d05/read-input filename)
        ctxs-tpl (repeat 5 (hash-map :ptr 0 :in '() :out '() :prog prog))]
      (->>
        (map
          (fn [pseq]
            (->>
              (map-indexed
                #(assoc %2 :in (list (nth pseq %1)))
                ctxs-tpl)
              (fb-loop "0")))
          phase-seqs)
        (reduce max))))

(p2 "data/d07-input.txt")
