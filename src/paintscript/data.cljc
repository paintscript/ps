(ns paintscript.data
  (:require [paintscript.nav :as nav]))

;; NOTE:
;; - unlike nav/Nav these are data paths for direct use with
;;   {get, update}-in and for simple acc/extension via conj
;; - {cmpt, el}-pthv are separate so they can be used on both cmpt-{root, sel}
(defrecord Loc [cmpt-pthv el-pthv])

(def locr-root (->Loc [] []))

(defn get-locr [locr field-k]
  (let [i (case field-k
            :src-k  0
            :x-el-k 1
            :p-el-i 2
            :xy-i   3)]
    (get-in locr [:el-pthv i])))

(defn locr->nav [locr]
  (reduce (fn [acc [k v]]
            (assoc acc k v))
          (nav/nav-rec :cmpt-pth (:cmpt-pthv locr))
          (map vector
               [:src-k :x-el-k :p-el-i :xy-i]
               (remove #{:el-argv} (:el-pthv locr)))))

;; NOTE: the pnt wrapper is only useful for the visual/interactive editor,
;; not for programmatic processing
(defrecord Pnt [locr pnt-k xy xy-norm pnt-i-main])
(defn pnt [& {:as args}] (map->Pnt args))

(defrecord Elem [locr el-src el-k el-opts el-argv])
(defn elem [& {:as args}] (map->Elem args))
(defn elemv [el-k el-argv] (elem :el-k el-k :el-argv el-argv))

(defn elv->r
  ([elv] (elv->r nil elv))
  ([locr [el-k arg1 :as elv]]
   (if (map? arg1)
     (->Elem locr elv el-k arg1 (subvec elv 2))
     (->Elem locr elv el-k nil  (subvec elv 1)))))

(defn elr->v
  [{:keys [el-k el-opts el-argv]}]
  (-> el-argv
      (cond->> el-opts (cons el-opts)
               el-k    (cons el-k))
      vec))

(defn parse-pcmd-pnts
  "parse path command args representing coordinates as Pnt records

   notes:
   - this representation is only needed for the visual/interactive editor and
     is not used for generic data-ops"
  [{:as pcmdr :keys [locr el-k el-argv]}]
  (case el-k

    ;; NOTE: only the final arg is a point
    ;; e.g. [:A [rx ry] [x-deg large sweep] xy]
    (:A :a) (let [locr' (-> locr (update :el-pthv conj :el-argv 2))
                  xy    (peek el-argv)]
              (list
               (pnt :locr locr' :xy xy)))

    ;; TODO: add a point for [:el-opts :xy]
    (:circle
     :ellipse
     :rect) (list)

    ;; else:
    (let [cp-cnt (-> el-argv count (- 1))
          pcmd-i (-> locr :el-pthv peek)]
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
             (pnt :locr locr' :pnt-k :pnt/main :xy xy))))
       el-argv))))

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
                                        (-> (elv->r locr' pcmdv)
                                            ; parse-pcmd-pnts
                                            )))))
                   nil)]
    (-> elr
        (cond-> el-opts' (assoc :el-opts el-opts')
                el-argv' (assoc :el-argv el-argv')))))

(defn- elrr->vv
  [{:as elr-tree :keys [el-k]}]
  (case el-k
    :path (-> elr-tree
              (update :el-argv #(mapv elr->v %))
              elr->v)
    (-> elr-tree
        elr->v)))

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



(defn serialize-cmpt
  [{:as cmpt-tree :keys [script], {cmpt-defs :components} :defs}]
  (-> cmpt-tree
      (cond-> cmpt-defs (assoc-in [:defs :components]
                                  (into {}
                                        (for [[cmpt-id
                                               cmpt-def] cmpt-defs]
                                          [cmpt-id
                                           (-> cmpt-def serialize-cmpt)])))
              script    (update :script #(mapv elrr->vv %)))))
