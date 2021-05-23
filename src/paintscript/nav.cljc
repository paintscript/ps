(ns paintscript.nav)

(def eli0 2) ;; offset for first el in path
(def xyi0 1) ;; offset for first pnt in el

(defn cmpt> [cmpt & {:keys [src-k pi ii iii] :or {src-k :script}}]
  (cond pi  (get-in cmpt [src-k pi])
        ii  (get-in cmpt (cons src-k ii))
        iii (get-in cmpt (cons src-k iii))))

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

;; --- rels

(defn els-prev [els & {:keys [eli eln]}]
  (get els (cond eli (-> eli (- eli0) (- 1))
                 eln (-> eln (- 1)))))

(defn els-next [els & {:keys [eli eln]}]
  (get els (-> eli (- eli0) (+ 1))))
