(ns d11
  (:require [d05]))

(defn update-ctx [ctx]
  (d05/ic-com ctx))

(defn update-direction [direction indicator]
  (cond
    (and (= direction :up) (= 0 indicator)) :left
    (and (= direction :up) (= 1 indicator)) :right
    (and (= direction :right) (= 0 indicator)) :up
    (and (= direction :right) (= 1 indicator)) :down
    (and (= direction :down) (= 0 indicator)) :right
    (and (= direction :down) (= 1 indicator)) :left
    (and (= direction :left) (= 0 indicator)) :down
    (and (= direction :left) (= 1 indicator)) :up))

(defn get-pos [x y direction]
  (cond
    (= :up direction) (hash-map :x x :y (inc y))
    (= :right direction) (hash-map :x (inc x) :y y)
    (= :down direction) (hash-map :x x :y (dec y))
    (= :left direction) (hash-map :x (dec x) :y y)))

; assume first panel is always current panel
(defn update-panels [panels color direction]
  (let [
        cur-panel (first panels)
        updated-panels (conj (drop 1 panels) (assoc cur-panel :color color))
        {x :x y :y} (get-pos (:x cur-panel) (:y cur-panel) direction)
        next-panel (reduce #(if (and (= x (:x %2)) (= y (:y %2))) (reduced %2) %1) nil updated-panels)]
    (if (nil? next-panel)
      (conj updated-panels (hash-map :x x :y y :color :black))
      (->>
        (remove #(and (= x (:x %)) (= y (:y %))) updated-panels)
        (#(conj % next-panel))))))

(defn run-robot [ctx direction panels cnt]
  (let [
        {ptr :ptr prog :prog} ctx]
    (if (or (= -1 cnt) (= '(\9 \9) (take-last 2 (nth prog ptr))))
      panels
      (let [
            in (list (if (= :black (:color (first panels))) "0" "1"))
            next-ctx (update-ctx (assoc ctx :in in))
            out (take-last 2 (:out next-ctx))
            color (if (= 0 (first out)) :black :white)
            next-direction (update-direction direction (second out))]
        (recur
          next-ctx
          next-direction
          (update-panels panels color next-direction)
          (inc cnt))))))

(defn p1 [filename]
  (run-robot
    (hash-map :ptr 0 :in '() :out '() :base 0 :prog (d05/read-input filename)) 
    :up
    (list (hash-map :x 0 :y 0 :color :black))
    0))

(defn p2 [filename]
  (run-robot
    (hash-map :ptr 0 :in '() :out '() :base 0 :prog (d05/read-input filename)) 
    :up
    (list (hash-map :x 0 :y 0 :color :white))
    0))

(defn main [args]
  (let [
        panels (p2 "data/d11-input.txt")
        xs (map #(:x %) panels)
        ys (map #(:y %) panels)
        min-x (reduce min xs)
        max-x (reduce max xs)
        min-y (reduce min ys)
        max-y (reduce max ys)
        num-rows (inc (- max-y min-y))
        num-cols (inc (- max-x min-x))
        canvas (vec (take (* num-rows num-cols) (repeat \.)))]
    (->>
      (reduce
        #(let [
               index (+ (- (:x %2) min-x) (* num-cols (- (:y %2) min-y)))
               color (if (= :white (:color %2)) \# \.)]
          (assoc %1 index color))
        canvas
        panels)
      (partition num-cols)
      (map #(reduce str %))
      (reverse)
      (run! prn))))
