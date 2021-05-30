(ns paintscript.data)

;; NOTE: unlike nav/Pth these are data paths for direct use with
;; {get, update}-in and for simple acc/extension via conj
(defrecord Loc [cmpt-pthv el-pthv])
(def locr-root (->Loc [] []))

(defrecord Pnt [locr pnt-k xy xy-norm pnt-i-main])
(defn pnt [& {:as args}] (map->Pnt args))

(defrecord Elem [locr el-src el-k el-opts el-argv])
(defn elem [& {:as args}] (map->Elem args))

(defn- elv->r
  [locr [el-k arg1 :as elv]]
  (if (map? arg1)
    (->Elem locr elv el-k arg1 (subvec elv 2))
    (->Elem locr elv el-k nil  (subvec elv 1))))

(defn- parse-pcmd
  "parse path command args representing coordinates as Pnt records"
  [{:as pcmdr :keys [locr el-k el-argv]}]
  (case el-k

    ;; NOTE: only the final arg is a point
    ;; e.g. [:A [rx ry] [x-deg large sweep] xy]
    (:A :a) (-> pcmdr
                (update-in [:el-argv 2]
                           (fn [xy]
                             (let [locr' (-> locr (update :el-pthv conj :el-argv 2))]
                               (pnt :locr locr' :xy xy)))))

    ;; TODO: add a point for [:el-opts :xy]
    (:circle
     :ellipse
     :rect) pcmdr

    ;; else:
    (let [cp-cnt (-> el-argv count (- 1))
          pcmd-i (-> locr :el-pthv peek)]
      (-> pcmdr
          (assoc :el-argv
                 (into []
                       (map-indexed
                        (fn [i xy]
                          (let [locr' (-> locr (update :el-pthv conj :el-argv i))]
                            (if (and ;(el/has-cp? p-el-k)
                                     (< i cp-cnt))
                              (let [i-main (if (and (= 2 cp-cnt)
                                                    (= 0 i))
                                             (dec pcmd-i)
                                             pcmd-i)]
                                (pnt :locr locr' :pnt-k :pnt/cp :xy xy :pnt-i-main i-main))
                              (pnt :locr locr' :pnt-k :pnt/main :xy xy)))))
                       el-argv))))))

(defn- elvv->rr
  [locr elv-tree]
  (let [{:as elr
         :keys [el-k
                el-opts
                el-argv]} (elv->r locr elv-tree)

        el-opts' (when-let [xy (:xy el-opts)]
                   (let [locr' (-> locr (update :el-pthv conj :el-opts :xy))]
                     (-> el-opts
                         (assoc :xy (pnt :locr locr' :xy xy)))))

        el-argv' (case el-k
                   :path (->> (map-indexed vector el-argv)
                              (mapv (fn [[i pcmdv]]
                                      (let [locr' (-> locr (update :el-pthv conj :el-argv i))]
                                        (-> (elv->r locr' pcmdv) parse-pcmd)))))
                   nil)]
    (-> elr
        (cond-> el-opts' (assoc :el-opts el-opts')
                el-argv' (assoc :el-argv el-argv')))))

(defn parse-cmpt
  ([cmpt-tree] (parse-cmpt locr-root cmpt-tree))
  ([locr {:as cmpt-tree :keys [script], {:keys [path-seqs
                                                components]} :defs}]
   (-> cmpt-tree
       (cond-> components
               (update-in [:defs :components]
                          #(into {}
                                 (map (fn [[cmpt-id
                                            cmpt-def]]
                                        (let [locr' (-> locr-root
                                                        (assoc :cmpt-pthv
                                                               (-> (:cmpt-pthv locr)
                                                                   (conj cmpt-id))))
                                              cmpt-def' (parse-cmpt locr' cmpt-def)]
                                          [cmpt-id cmpt-def'])))
                                 %))
               script
               (update :script #(into []
                                      (map-indexed
                                       (fn [i elv]
                                         (let [locr' (-> locr
                                                         (assoc :el-pthv [:script i]))]
                                           (elvv->rr locr' elv))))
                                      %))))))
