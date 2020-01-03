(ns paintscript.nav)

(def eli0 2)
(def xyi0 1)

(defn script> [script & {:keys [pi ii iii]}]
  (cond pi  (get-in script [:script pi])
        ii  (get-in script (cons :script ii))
        iii (get-in script (cons :script iii))))

(defn p> [p & {:keys [eli eln]}]
  (cond eli (get p eli)
        eln (get p (+ eln eli0))))

(defn els> [els & {:keys [eli eln]}]
  (cond eli (nth els (- eli eli0))
        eln (nth els eln)))

(defn el> [el & {:keys [xyi xyn]}]
  (cond xyi (get el xyi)
        xyn (get el (+ xyn xyi0))))

(defn xys> [xys & {:keys [xyi xyn]}]
  (cond xyi (nth xys (- xyi xyi0))
        xyn (nth xys xyn)))

;; rels

(defn els-prev [els & {:keys [eli]}]
  (nth els (-> eli (- eli0) (- 1))))

(defn els-next [els & {:keys [eli]}]
  (nth els (-> eli (- eli0) (+ 1))))
