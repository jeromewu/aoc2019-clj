(ns lab
  (:import jline.Terminal))

(defn main [args]
  (let [term (Terminal/getTerminal)]
    (while true
      (->>
        (.readCharacter term System/in)
        (#(cond (= 106 %) (prn "j") :else (prn "not j")))))))

(conj '[1 2 3] 4)
