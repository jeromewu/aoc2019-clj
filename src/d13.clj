(ns d13
  (:import jline.Terminal)
  (:require [d05]))

(defn read-in [filename]
  (->>
    (slurp filename)
    (re-seq #"[\d|-]+")
    (vec)))

(defn snapshot-in [filename in]
  (->>
    (reduce #(str %1 "," %2) in)
    (spit filename)))

(defn p1 [filename]
  (->>
    (d05/ic-com (hash-map :ptr 0 :in [] :out [] :base 0 :prog (d05/read-input filename)))
    (:out)
    (partition 3)
    (filter #(= 2 (nth % 2)))
    (count)))

; empty: ., wall: x, block: #, horizontal paddle: -, ball: o
(defn print-tiles [tiles]
  (let [
        width (->> (map #(nth % 0) tiles) (reduce max) (inc))
        height (->> (map #(nth % 1) tiles) (reduce max) (inc))
        score (->> (filter #(= -1 (nth % 0)) tiles) (last))
        screen (vec (repeat (* width height) \.))]
    (prn score)
    (->>
      (reduce
        #(
          let [
               [x y tid] %2
               idx (+ x (* y width))]
            (if (neg? x)
              %1
              (cond
                (= 0 tid) (assoc %1 idx \.)
                (= 1 tid) (assoc %1 idx \x)
                (= 2 tid) (assoc %1 idx \#)
                (= 3 tid) (assoc %1 idx \-)
                (= 4 tid) (assoc %1 idx \o))))
        screen
        tiles)
      (partition width)
      (map #(reduce str %))
      (run! prn))))

(defn play-game [ctx prev-in]
  (let [
        term (Terminal/getTerminal)
        in (->> (.readCharacter term System/in) (#(cond (= 106 %) "-1" (= 107 %) "0" (= 108 %) "1" :else "0")))
        next-in (conj prev-in in)
        ctx-in (assoc ctx :in (conj '[] in))
        next-ctx (d05/ic-com ctx-in)
        tiles (->> (:out next-ctx) (partition 3))
        _ (snapshot-in "data/d13-snapshot.txt" next-in)]
    (print-tiles tiles)
    (recur next-ctx next-in)))

(defn p2 [filename in]
  (prn "Use `j` to go left, `k` to stay and `l` to right")
  (let [
        ctx (d05/ic-com (hash-map :ptr 0 :in in :out '[] :base 0 :prog (d05/read-input filename)))]
    (->> (:out ctx) (partition 3) (print-tiles))
    (play-game ctx in)))

(defn main [args]
  (prn (p1 "data/d13-input.txt"))
  (p2 "data/d13-input.txt" (read-in "data/d13-snapshot.txt")))
