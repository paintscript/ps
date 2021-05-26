(ns paintscript.nav
  (:require [paintscript.util :as u]))

(defrecord Pth [cmpt-pth
                src-k
                x-el-k
                p-el-i
                xy-i])

(defn pth-rec [& {:as args :keys [cmpt-pth src-k x-el-k p-el-i xy-i]}]
  {:pre [(case src-k
           :script (-> x-el-k integer?)
           :defs   (-> x-el-k string?)
           true)
         (-> p-el-i ((some-fn nil? integer?)))
         (-> xy-i   ((some-fn nil? integer?)))]}
  (map->Pth args))

(defn pth-vec->rec [[src-k x-el-k p-el-i xy-i]]
  (pth-rec :src-k  src-k
           :x-el-k x-el-k
           :p-el-i p-el-i
           :xy-i   xy-i))

(defn- cmpt-pth->data-pth [cmpt-pth]
  (mapcat (fn [cmpt-id]
            (list :defs :components cmpt-id))
          cmpt-pth))

(defn pth-rec->vec
  [{:as pth-rec :keys [cmpt-pth src-k x-el-k p-el-i xy-i]}]
  {:pre [(instance? Pth pth-rec)]}
  (concat (-> cmpt-pth
              cmpt-pth->data-pth)
          (remove nil? [src-k x-el-k p-el-i xy-i])))

(defn pth-up
  [{:as pth-rec :keys [cmpt-pth src-k x-el-k p-el-i xy-i]}]
  (reduce (fn [_ k]
            (when (get pth-rec k)
              (reduced (-> pth-rec (assoc k nil)))))
          nil
          [:xy-i :p-el-i :x-el-k :src-k :cmpt-pth]))

(defn get-cmpt-sel [cmpt {:as sel-rec :keys [cmpt-pth]}]
  (-> cmpt
      (cond-> cmpt-pth
              (get-in (-> cmpt-pth
                          cmpt-pth->data-pth)))))

(defn cmpt-merged [cmpt cmpt-root {:as sel-rec :keys [cmpt-pth]}]
  (-> cmpt
      (cond-> cmpt-pth
              (merge (u/cascading-merges cmpt-root
                                         (->> cmpt-pth
                                              (map (fn [cmpt-id]
                                                     (cmpt-pth->data-pth [cmpt-id]))))
                                         [:config
                                          :canvas
                                          :defs
                                          :attr-classes])))))

(defn xy-pth? [pth-rec]
  (:xy-i pth-rec))

(def p-el-i0 2) ;; offset for first path-el in path
(def xy-i0   1) ;; offset for first xy in path-el

(defn cmpt> [cmpt & {:keys [src-k s-eli ii iii] :or {src-k :script}}]
  (cond s-eli (get-in cmpt [src-k s-eli])
        ii    (get-in cmpt (cons src-k ii))
        iii   (get-in cmpt (cons src-k iii))))

(defn s-el>
  "given a script element `s-el` of type :path select one of its path elements"
  [s-el & {:keys [p-eli
                  p-eln]}]
  (cond p-eli (get s-el p-eli)
        p-eln (get s-el (+ p-eln p-el-i0))))

;; TODO: path elements?
(defn els> [els & {:keys [eli eln]}]
  (cond eli (nth els (- eli p-el-i0))
        eln (nth els eln)))

;; TODO: path element?
(defn el> [el & {:keys [xyi xyn]}]
  (cond xyi (get el xyi)
        xyn (get el (+ xyn xy-i0))))

(defn xys> [xys & {:keys [xyi xyn]}]
  (cond xyi (nth xys (- xyi xy-i0))
        xyn (nth xys xyn)))

;; --- rels

;; TODO: path elements?
(defn els-prev [els & {:keys [eli eln]}]
  (get els (cond eli (-> eli (- p-el-i0) (- 1))
                 eln (-> eln (- 1)))))

(defn els-next [els & {:keys [eli eln]}]
  (get els (-> eli (- p-el-i0) (+ 1))))
