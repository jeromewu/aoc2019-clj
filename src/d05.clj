(ns d05)

(defn assoc-col [index v _col]
  (let [
        len (count _col)
        diff (- (inc index) len)
        col (if (pos? diff) (into _col (vec (repeat diff "0"))) _col)]
    (assoc col index (str v))))

(defn read-input [filename]
  (->>
    (slurp filename)
    (re-seq #"[\d|-]+")
    (vec)))

(defn pad [opcode]
  (->>
    (take-last 5 (str "00000" opcode))))

(defn get-pars [ctx]
  (let [
        {ptr :ptr prog :prog base :base} ctx
        opcode (pad (nth prog ptr))
        p1 (bigint (nth prog (+ 1 ptr) "0"))
        p2 (bigint (nth prog (+ 2 ptr) "0"))
        p1-ref (bigint (if (<= p1 Integer/MAX_VALUE) (nth prog p1 "0") "0"))
        p2-ref (bigint (if (<= p2 Integer/MAX_VALUE) (nth prog p2 "0") "0"))
        p1-rel (bigint (if (<= (+ base p1) Integer/MAX_VALUE) (nth prog (+ base p1) "0") "0"))
        p2-rel (bigint (if (<= (+ base p2) Integer/MAX_VALUE) (nth prog (+ base p2) "0") "0"))
        p3 (bigint (nth prog (+ 3 ptr) "0"))
        p3-rel (+ base p3)]
    (hash-map :opcode opcode :p1 p1 :p2 p2 :p1-ref p1-ref :p2-ref p2-ref :p1-rel p1-rel :p2-rel p2-rel :p3 p3 :p3-rel p3-rel)))

(defn nth-par [pars n]
  (let [
        {opcode :opcode
         p1 :p1
         p2 :p2
         p1-ref :p1-ref
         p2-ref :p2-ref
         p1-rel :p1-rel
         p2-rel :p2-rel
         p3 :p3
         p3-rel :p3-rel} pars]
    (cond
      (= 1 n) (cond
                (= \0 (nth opcode 2)) p1-ref
                (= \1 (nth opcode 2)) p1
                (= \2 (nth opcode 2)) p1-rel)
      (= 2 n) (cond
                (= \0 (nth opcode 1)) p2-ref
                (= \1 (nth opcode 1)) p2
                (= \2 (nth opcode 1)) p2-rel)
      (= 3 n) (cond
                (= \0 (nth opcode 0)) p3
                (= \1 (nth opcode 0)) p3
                (= \2 (nth opcode 0)) p3-rel))))

(defn update-ptr [ctx pars]
  (let [
        {ptr :ptr} ctx
        {opcode :opcode} pars
        np (partial nth-par pars)
        p1 (np 1)
        p2 (np 2)
        last-2-digits (take-last 2 opcode)]
    (cond
      (= last-2-digits '(\0 \1)) (+ 4 ptr)
      (= last-2-digits '(\0 \2)) (+ 4 ptr)
      (= last-2-digits '(\0 \3)) (+ 2 ptr)
      (= last-2-digits '(\0 \4)) (+ 2 ptr)
      (= last-2-digits '(\0 \5)) (if-not (zero? p1) p2 (+ 3 ptr))
      (= last-2-digits '(\0 \6)) (if (zero? p1) p2 (+ 3 ptr))
      (= last-2-digits '(\0 \7)) (+ 4 ptr)
      (= last-2-digits '(\0 \8)) (+ 4 ptr)
      (= last-2-digits '(\0 \9)) (+ 2 ptr))))

(defn update-input [ctx pars]
  (let [
        {in :in} ctx
        {opcode :opcode} pars]
    (cond
      (= (take-last 2 opcode) '(\0 \3)) (drop 1 in)
      :else in)))

(defn update-out [ctx pars]
  (let [
        {out :out} ctx
        {opcode :opcode} pars
        np (partial nth-par pars)]
    (if (= (take-last 2 opcode) '(\0 \4))
      (conj out (np 1))
      out)))

(defn update-base [ctx pars]
  (let [
        {base :base} ctx
        {opcode :opcode} pars
        np (partial nth-par pars)]
    (if (= (take-last 2 opcode) '(\0 \9))
      (+ base (np 1))
      base)))

(defn update-prog [ctx pars]
  (let [
        {prog :prog
         base :base
         in :in} ctx
        {opcode :opcode
         p1 :p1} pars
        np (partial nth-par pars)
        last-2-digits (take-last 2 opcode)]
    (cond
      (= last-2-digits '(\0 \1)) (assoc-col (np 3) (+ (np 1) (np 2)) prog)
      (= last-2-digits '(\0 \2)) (assoc-col (np 3) (* (np 1) (np 2)) prog)
      (= last-2-digits '(\0 \3)) (assoc-col (if (= \2 (nth opcode 2)) (+ base p1) p1) (first in) prog)
      (= last-2-digits '(\0 \7)) (assoc-col (np 3) (if (< (np 1) (np 2)) 1 0) prog)
      (= last-2-digits '(\0 \8)) (assoc-col (np 3) (if (= (np 1) (np 2)) 1 0) prog)
      :else prog)))

(defn run-prog [ctx]
  (let [
        {in :in} ctx
        pars (get-pars ctx)
        {opcode :opcode} pars
        last-2-digits (take-last 2 opcode)]
        ; _ (prn (:in ctx))
        ; _ (prn (:base ctx))
        ; _ (prn (:prog ctx))
        ; _ (prn pars)]
        ; _ (prn (nth (:prog ctx) 3))]
    (cond
      (and (= last-2-digits '(\0 \3)) (empty? in)) ctx
      (= last-2-digits '(\9 \9)) ctx
      :else (recur
              (hash-map
                :ptr (update-ptr ctx pars)
                :in (update-input ctx pars)
                :out (update-out ctx pars)
                :base (update-base ctx pars)
                :prog (update-prog ctx pars))))))

(defn ic-com [ctx]
  (run-prog ctx))

(defn p1 [filename in]
  (->>
    (hash-map :ptr 0 :in in :out [] :base 0 :prog (read-input filename))
    (ic-com)
    (:out)
    (last)))

(p1 "data/d05-input0.txt" ["8"])
