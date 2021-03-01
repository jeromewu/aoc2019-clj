(ns d14)

(defn col-to-map [col]
  (reduce #(let [[v k] %2] (assoc %1 k (bigint v))) {} col))

(defn make-entry [reaction]
  (let [
        [qty chem] (last reaction)
        out (drop-last reaction)]
    (hash-map
      chem
      (hash-map
        :qty (bigint qty)
        :out (col-to-map out)))))

(defn read-input [filename]
  (->>
    (slurp filename)
    (re-seq #".+")
    (map #(re-seq #"\w+" %))
    (map #(partition 2 %))
    (map #(make-entry %))
    (reduce merge)
    (#(assoc % "ORE" {:qty 1 :out {"ORE" 1}}))))

(defn multiply-map [v m]
  (->>
    (map #(let [[k _v] %] [k (* v _v)]) m)
    (into {})))

(defn reaction-completed? [required-chems]
  (and (= 1 (count required-chems)) (contains? required-chems "ORE")))

(defn revert-chem [reactions entry]
  (let [
        chem (key entry)
        qty (val entry)
        reaction (reactions chem)
        unit-qty (:qty reaction)
        unit (bigint (Math/ceil (/ qty unit-qty)))
        remain (- (* unit unit-qty) qty)
        out (multiply-map unit (:out reaction))]
        ; _ (prn chem qty)
        ; _ (prn out chem remain)]
    (cond
      (zero? remain) (list out {})
      :else (list out (hash-map chem remain)))))

(defn revert-chems [reactions chems]
  (reduce
    #(let [
           ; _ (prn %1)
           [reverted-chems remain-chem] (revert-chem reactions %2)]
       (hash-map
         :required (merge-with + reverted-chems (:required %1))
         :remained (merge-with + remain-chem (:remained %1))))
    {:required {} :remained (:remained chems)}
    (:required chems)))

(defn use-remained-chems [chems]
  (reduce
    #(let [
           {required :required remained :remained} %1
           [c q] %2]
       (if (contains? remained c)
         (hash-map
           :required (assoc required c (- q (remained c)))
           :remained (dissoc remained c))
         (hash-map
           :required (assoc required c q)
           :remained remained)))
    {:required {} :remained (:remained chems)}
    (:required chems)))
        
(defn get-num-ores [reactions chems]
  (if (reaction-completed? (:required chems))
    (get (:required chems) "ORE")
    (->>
      (revert-chems reactions chems)
      (use-remained-chems)
      (recur reactions))))

(defn p1 [filename num-fuels]
  (let [reactions (read-input filename)]
    (get-num-ores
      reactions
      {:required (hash-map "FUEL" num-fuels) :remained {}})))

(def one-tri 1000000000000)

(defn get-max-fuels [filename min-fuels max-fuels]
  (let [
        num-fuels (/ (+ min-fuels max-fuels) 2)
        get-num-ores (partial p1 filename)
        ores (get-num-ores num-fuels)
        next-ores (get-num-ores (inc num-fuels))]
        ; _ (prn (bigint num-fuels))]
    (if (and (<= ores one-tri) (> next-ores one-tri))
      (bigint (inc num-fuels))
      (if (<= ores one-tri)
        (recur filename num-fuels max-fuels)
        (recur filename min-fuels num-fuels)))))

(defn p2 [filename]
  (let [
        ores-per-fuel (p1 filename 1)
        min-fuels (bigint (/ one-tri ores-per-fuel))]
    (get-max-fuels filename min-fuels one-tri)))
        
(defn main [args]
  (prn (p1 "data/d14-input.txt" 1))
  (prn (p2 "data/d14-input.txt")))
