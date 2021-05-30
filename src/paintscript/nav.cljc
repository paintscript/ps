(ns paintscript.nav
  (:require [paintscript.util :as u]))

(defrecord Pth [cmpt-pth0 ;; base cmpt
                ref-pth   ;; chain of references (starting from cmpt-base)
                cmpt-pth  ;; effective cmpt-pth after resolving references

                src-k     ;; :script or :defs (only used for path-seq defs)
                x-el-k    ;; main-element index (within :script or :defs)
                p-el-i    ;; sub-element index (within :path)
                xy-i      ;; point index (within :path element)
                ])

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
  ([pthr] (pth-rec->vec pthr nil))
  ([{:as pthr :keys [cmpt-pth src-k x-el-k p-el-i xy-i]} trunc-k]
   {:pre [(instance? Pth pthr)]}
   (concat (-> cmpt-pth
               cmpt-pth->data-pth)

           ;; truncate on nil or trunc-k
           (reduce (fn [pthv k]
                     (let [v (get pthr k)]
                       (-> pthv
                           (cond-> v (conj v)
                                   (or (not v)
                                       (= trunc-k k)) reduced))))
                   []
                   kk-levels))))

(defn truncate-pth [pthr k-last]
  (loop [[k & kk] kk-all
         trunc?   false
         pthr     pthr]
    (cond
      (not k)      pthr
      (= k-last k) (recur kk true pthr)
      :else        (let [pthr (-> pthr
                                  (cond-> trunc? (assoc k nil)))]
                     (recur kk trunc? pthr)))))

(defn get-in-pth
  ([cmpt-root pthr] (get-in-pth cmpt-root pthr nil))
  ([cmpt-root pthr trunc-k]
   (get-in cmpt-root (pth-rec->vec pthr trunc-k))))

(defn update-in-pth [cmpt-root pthr f & args]
  (apply update-in cmpt-root (pth-rec->vec pthr) f args))

(defn update-in-pth* [cmpt-root pthr trunc-k f & args]
  (if-not (get pthr trunc-k)
    cmpt-root
    (apply update-in cmpt-root (pth-rec->vec pthr trunc-k) f args)))

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
             cmpt-sub      (get-in cmpt-root
                                   (-> cmpt-sub-pth
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

(defn- pth-head-k [pthr]
  (->> kk-rev
       (filter #(get pthr %))
       first))

(defn pth-up [pthr & {:keys [drop-src-k?]}]
  (when-let [k (pth-head-k pthr)]
    (-> pthr
        (assoc k nil)
        (cond-> (and (= :x-el-k k)
                     drop-src-k?)
                (assoc :src-k nil)

                (and (= :cmpt-pth k)
                     (:ref-pth pthr))
                (update :ref-pth butlast)))))

(defn pth-max [pthr cmpt]
  (-> (get-in-pth cmpt (-> pthr
                           pth-up))
      count
      (- 1)))

(defn pth-next [pthr cmpt]
  (if-let [k (pth-head-k pthr)]
    (-> pthr
        (update k #(-> % inc (min (pth-max pthr cmpt)))))
    (pth-rec :src-k :script
             :x-el-k 0)))

(defn pth-prev [pthr]
  (when-let [k (pth-head-k pthr)]
    (-> pthr
        (update k #(-> % dec (max (case k
                                    :p-el-i 2
                                    :xy-i 1
                                    0)))))))

(defn sel-ref [sel-rec cmpt [_s-el-k
                             s-el-opts
                             cmpt-id]]
  (let [ref-item  (-> s-el-opts (assoc :cmpt-id cmpt-id))
        ref-pth'  (-> (:ref-pth sel-rec) (u/conjv ref-item))
        cmpt-pth0 (:cmpt-pth0 sel-rec)
        cmpt-pth  (ref-pth->cmpt-pth cmpt cmpt-pth0 ref-pth')]
    (pth-rec
     :cmpt-pth0 cmpt-pth0
     :ref-pth   ref-pth'
     :cmpt-pth  cmpt-pth)))

(defn pth-down [pthr cmpt]
  (when-let [k (pth-head-k pthr)]
    (case k
      :cmpt-pth (-> pthr (assoc :src-k  :script
                                :x-el-k 0))
      :x-el-k   (let [[x-el-k :as ref-el] (get-in-pth cmpt pthr)]
                  (case x-el-k
                    :ref (-> pthr (sel-ref cmpt ref-el))
                    (-> pthr (assoc :p-el-i 2))))
      :p-el-i   (-> pthr (assoc :xy-i   1))
      pthr)))

(defn xy-pth? [pthr]
  (:xy-i pthr))

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
