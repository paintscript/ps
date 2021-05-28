(ns paintscript.nav
  (:require [paintscript.util :as u]))

(defrecord Pth [cmpt-pth0 ref-pth
                cmpt-pth
                src-k
                x-el-k
                p-el-i
                xy-i])

(defn pth-rec [& {:as args :keys [ cmpt-pth src-k x-el-k p-el-i xy-i]}]
  {:pre [(case src-k
           :script (-> x-el-k integer?)
           :defs   (-> x-el-k string?)
           true)
         (-> p-el-i ((some-fn nil? integer?)))
         (-> xy-i   ((some-fn nil? integer?)))]}
  (map->Pth args))

(defn pth-vec->rec
  ;; NOTE: only used for xspace
  [[src-k x-el-k p-el-i xy-i]]
  (pth-rec :src-k  src-k
           :x-el-k x-el-k
           :p-el-i p-el-i
           :xy-i   xy-i))

(defn- cmpt-pth->data-pth [cmpt-pth]
  (mapcat (fn [cmpt-id]
            (list :defs :components cmpt-id))
          cmpt-pth))

(def kk-levels [:src-k :x-el-k :p-el-i :xy-i])
(def kk-all    [:cmpt-pth :src-k :x-el-k :p-el-i :xy-i])
(def kk-rev    (reverse kk-all))

(defn pth-rec->vec
  ([pth-rec] (pth-rec->vec pth-rec nil))
  ([{:as pth-rec :keys [cmpt-pth src-k x-el-k p-el-i xy-i]} trunc-k]
   {:pre [(instance? Pth pth-rec)]}
   (concat (-> cmpt-pth
               cmpt-pth->data-pth)

           ;; truncate on nil or trunc-k
           (reduce (fn [pthv k]
                     (let [v (get pth-rec k)]
                       (-> pthv
                           (cond-> v (conj v)
                                   (or (not v)
                                       (= trunc-k k)) reduced))))
                   []
                   kk-levels))))

(defn pth-up
  [{:as pth-rec :keys [cmpt-pth src-k x-el-k p-el-i xy-i]}]
  (reduce (fn [_ k]
            (when (get pth-rec k)
              (reduced (-> pth-rec (assoc k nil)))))
          nil
          kk-rev))

(defn truncate-pth [pth-rec k-last]
  (loop [[k & kk] kk-all
         trunc?   false
         pth-rec  pth-rec]
    (cond
      (not k)      pth-rec
      (= k-last k) (recur kk true pth-rec)
      :else        (let [pth-rec (-> pth-rec
                                     (cond-> trunc? (assoc k nil)))]
                     (recur kk trunc? pth-rec)))))

(defn get-in-pth [cmpt-root pth-rec trunc-k]
  (get-in cmpt-root (pth-rec->vec pth-rec trunc-k)))

(defn update-in-pth [cmpt-root pth-rec f & args]
  (apply update-in cmpt-root (pth-rec->vec pth-rec) f args))

(defn update-in-pth* [cmpt-root pth-rec trunc-k f & args]
  (apply update-in cmpt-root (pth-rec->vec pth-rec trunc-k) f args))

(defn- cmpt->index [cmpt-pth cmpt]
  (->> (keys (get-in cmpt [:defs :components]))
       (map (fn [cmpt-id]
              [cmpt-id (conj cmpt-pth cmpt-id)]))
       (into {})))

(defn ref-pth->cmpt-pth
  ([cmpt-root ref-pth] (ref-pth->cmpt-pth cmpt-root nil ref-pth))
  ([cmpt-root cmpt-pth0 ref-pth]
   (loop [cmpt (-> cmpt-root
                   (cond-> cmpt-pth0
                           (get-in (-> cmpt-pth0
                                       cmpt-pth->data-pth))))
          [{:as ref :keys [cmpt-id]}
           & ref-pth-tail] ref-pth
          cmpt-pth     (or cmpt-pth0 [])
          cmpt-id->pth (cmpt->index [] cmpt-root)]
     (if-not cmpt-id
       cmpt-pth
       (let [cmpt-sub-pth  (cmpt-id->pth cmpt-id)
             cmpt-sub      (get-in cmpt (-> cmpt-sub-pth
                                            cmpt-pth->data-pth))
             cmpt-id->pth' (-> cmpt-id->pth (merge (cmpt->index cmpt-sub-pth cmpt-sub)))]
         (recur cmpt-sub
                ref-pth-tail
                (get cmpt-id->pth' cmpt-id)
                cmpt-id->pth'))))))

(defn get-cmpt-sel [cmpt-root {:as sel-rec :keys [cmpt-pth0
                                                  ref-pth
                                                  cmpt-pth]}]
  (let [cmpt-base (-> cmpt-root
                      (cond-> cmpt-pth0
                              (get-in (-> cmpt-pth0
                                          cmpt-pth->data-pth))))
        cmpt-sel  (if-not ref-pth
                    cmpt-base
                    (-> cmpt-root
                        (cond-> cmpt-pth
                                (get-in (-> cmpt-pth
                                            cmpt-pth->data-pth)))))]
    [cmpt-base
     cmpt-sel]))

(defn cmpt-merge-canvas [cmpt cmpt-root {:as sel-rec :keys [cmpt-pth]}]
  (-> cmpt
      (cond-> cmpt-pth
              (merge (u/cascading-merges cmpt-root
                                         (->> cmpt-pth
                                              (map (fn [cmpt-id]
                                                     (cmpt-pth->data-pth [cmpt-id]))))
                                         [:canvas])))))

(defn cmpt-merge-defs [cmpt cmpt-root {:as sel-rec :keys [cmpt-pth]}]
  (-> cmpt
      (cond-> cmpt-pth
              (merge (u/cascading-merges cmpt-root
                                         (->> cmpt-pth
                                              (map (fn [cmpt-id]
                                                     (cmpt-pth->data-pth [cmpt-id]))))
                                         [
                                          ; :config
                                          ; :canvas
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
