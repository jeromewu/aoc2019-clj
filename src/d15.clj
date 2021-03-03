(ns d15
  (:require [d05]))

(def unit-map {"1" [0 1] "2" [0 -1] "3" [-1 0] "4" [1 0]})
(def dirs ["1" "2" "3" "4"])

(defn find-tank [ctx pos visited steps status]
  (reduce
    #(let [
           [ctx pos visited steps status] %1
           dir %2
           ctx' (d05/ic-com (assoc ctx :in (vector dir)))
           pos' (map + pos (unit-map dir))
           visited' (conj visited pos)
           status' (last (:out ctx'))
           steps' (inc steps)]
      (cond
        (or (contains? visited' pos') (= 0 status')) [ctx pos visited' steps status]
        (= 2 status') (reduced [ctx' pos' visited' steps' status'])
        (= 1 status')
        (let [
              ret (find-tank ctx' pos' visited' steps' status')
              status'' (last ret)
              visited'' (nth ret 2)]
          (if (= 2 status'')
            (reduced ret)
            [ctx pos visited'' steps status]))))
    [ctx pos visited steps status]
    dirs))

(defn get-tiles [ctx pos visited status]
  (reduce
    #(let [
           [ctx pos visited status] %1
           dir %2
           ctx' (d05/ic-com (assoc ctx :in (vector dir)))
           pos' (vec (map + pos (unit-map dir)))
           visited' (assoc visited pos status)
           status' (last (:out ctx'))]
      (cond
        (or (contains? visited' pos') (= 0 status')) [ctx pos (assoc visited' pos' status') status]
        :else
        (let [
              ret (get-tiles ctx' pos' visited' status')]
          [ctx pos (nth ret 2) status])))
    [ctx pos visited status]
    dirs))

(defn p1 [filename]
  (->>
    (find-tank
      {:ptr 0 :in [] :out [] :base 0 :prog (d05/read-input filename)}
      [0 0]
      #{}
      0
      1N)
    (#(nth % 3))))

(defn get-poss' [poss tiles]
  (reduce
    (fn [poss' pos]
      (concat
        poss'
        (reduce 
          #(let [
                 pos' (vec (map + pos %2))]
             (if (= 1 (get tiles pos'))
               (conj %1 pos')
               %1))
          []
          (vals unit-map))))
    []
    poss))

(defn get-mins [poss tiles mins]
  (let [
        tiles' (reduce #(assoc %1 %2 3N) tiles poss)
        poss' (get-poss' poss tiles')]
    (if (= 0 (count poss'))
      mins
      (recur poss' tiles' (inc mins)))))

(defn p2 [filename]
  (let [
        ctx {:ptr 0 :in [] :out [] :base 0 :prog (d05/read-input filename)}
        tiles (nth (get-tiles ctx [0 0] {} 1N) 2)
        tank-pos (reduce #(if (= 2 (val %2)) (reduced (key %2)) %1) nil tiles)]
    (get-mins (vector tank-pos) tiles 0)))

(defn main [args]
  (prn (p1 "data/d15-input.txt"))
  (prn (p2 "data/d15-input.txt")))
