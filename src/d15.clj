(ns d15
  (:require [d05]))

(def unit-map {"1" [0 1] "2" [0 -1] "3" [-1 0] "4" [1 0]})
(def dirs ["1" "2" "3" "4"])

(defn find-tank [ctx pos visited]
  (reduce
    #(let [
           next-ctx (d05/ic-com (assoc ctx :in (vector %2)))
            out (last (:out next-ctx))]
      (cond
        (= "0" out) ()
        (= "1" out) ()
        (= "2" out) ()))
    visited
    dirs))

(defn main [args]
  ())
