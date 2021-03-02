(ns d15
  (:require [d05]))

(def unit-map {"1" [0 1] "2" [0 -1] "3" [-1 0] "4" [1 0]})
(def dirs ["1" "2" "3" "4"])

(defn find-tank [ctx pos visited status]
  (reduce
    #(let [
           dir %2
           ctx' (d05/ic-com (assoc ctx :in (vector dir)))
           pos' (map + pos (unit-map dir))
           visited' (conj visited pos)
           status' (last (:out ctx'))
           _ (prn dir status' pos pos')]
      (cond
        (or (= 2 status') (= 2 status)) [ctx' pos' visited' status]
        (contains? visited pos') [ctx pos visited' status]
        (= 0 status') [ctx pos visited' status]
        (= 1 status') (find-tank ctx' pos' visited' status')))
    [ctx pos visited status]
    dirs))

(defn p1 [filename]
  (->>
    (find-tank
      {:ptr 0 :in [] :out [] :base 0 :prog (d05/read-input filename)}
      [0 0]
      #{}
      1)
    (second)))

(defn main [args]
  (p1 "data/d15-input.txt"))
