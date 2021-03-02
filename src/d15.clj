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
      1)
    (#(nth % 3))))

(defn p2 [filename]
  (->>
    (get-tiles
      {:ptr 0 :in [] :out [] :base 0 :prog (d05/read-input filename)}
      [0 0]
      {}
      1N)
    (#(nth % 2))))

(defn main [args]
  (prn (p1 "data/d15-input.txt"))
  (prn (p2 "data/d15-input.txt")))
