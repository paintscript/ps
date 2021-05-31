(ns paintscript.data-ops
  (:require [paintscript.util :as u]
            [paintscript.nav :as nav]
            [paintscript.data :as data]
            [paintscript.data-ops-path :as el]))

;; -----------------------------------------------------------------------------
;; normalize

(defn normalize-pcmd-seq
  [pcmd-seq & {:keys [op] :or {op :all}}]
  {:pre [(when op (-> op #{:all :rel->abs :short->full}))]}
  (loop [[{:as el :keys [el-k]} & els-tail] pcmd-seq
         tgt-prev nil
         cp-prev  nil
         acc      []]
    (if-not el-k
      acc
      (let [[el' tgt' cp']
            (reduce (fn [[el _tgt-prev _cp-prev] [f-k f-step]]
                      (-> (f-step el tgt-prev cp-prev)
                          (update 0 el/merge-el-tup el)))
                    [el tgt-prev cp-prev]
                    (el/normalization-steps el-k op))
            el'' (-> el' (with-meta (meta el)))]
        (recur els-tail tgt' cp' (conj acc el''))))))

;; -----------------------------------------------------------------------------
;; transform

;; --- reverse

(defn- reverse-p-el-xys
  "drop last point (implicit) and redistribute the rest in reverse:
    ([:M 1] [:L 2 3] [:C 4 5 6] [:C 7 8 9])
    ~> ([:C 7 8 9] [:C 4 5 6] [:L 2 3] [:M 1])
    => ([:C 8 7 6] [:C 5 4 3] [:L 2 1])
  "
  [{:keys [drop-last?]} els]
  ;; TODO: handle (:Z :z)
  (loop [[el & els-tail] els
         acc      []
         tgt-prev nil]
    (if-not el
      (-> (reverse acc)
          (cond-> (not drop-last?)
                  (conj (data/elem :el-k    :M
                                   :el-argv [tgt-prev]))))
      (let [[el' tgt] (el/el->reversed el tgt-prev)]
        (recur els-tail (-> acc (cond-> el' (conj el'))) tgt)))))

(defn reverse-p-els
  ([els] (reverse-p-els nil els))
  ([opts els]
   (->> els
        normalize-pcmd-seq
        (reverse-p-el-xys opts))))

;; --- mirror

(defn- mirror-xy  [axis pos pnt] (update pnt axis #(- pos %)))
(defn- mirror-xys [axis pos xys] (map #(mirror-xy axis pos %) xys))

(defn mirror-p-els [axis pos els]
  (->> els
       (mapv (fn [{:as el :keys [el-k el-opts], xys :el-argv}]
               (case el-k
                 :arc (-> el
                          (update-in [:el-opts :mode]
                                     #(case % :convex :concave :convex))
                          (update :el-argv
                                  #(mirror-xys axis pos %)))
                 :A (let [[r [rot f1 f2] xy] xys

                          ;; NOTE: needs to be undone when combined with reverse
                          f2' (-> f2 el/flip-bin)]
                      (-> el
                          (assoc :el-argv [r [rot f1 f2'] (mirror-xy axis pos xy)])))
                 (-> el
                     (update :el-argv #(mirror-xys axis pos %))))))))

(defn add-mirrored
  "subdivide els in chunks terminated w/ :z and append a mirrored version after each"
  [mode axis pos els]
  (let [els-parts
        (as-> (partition-by (comp #{:Z :z} :el-k) els) coll
              (if (odd? (count coll))
                (-> coll (concat [nil]))
                coll)
              (partition 2 coll))]
    (sequence
     (comp (map #(apply concat %))
           (mapcat
            (fn [els-part]
              (concat
               els-part
               (->> (case mode
                      :merged   (->> els-part (reverse-p-els {:drop-last? true}))
                      :separate (->  els-part
                                     (normalize-pcmd-seq
                                      ;; - :rel->abs for math
                                      ;; - :short->full for uniform data (esp. :arc)
                                      :op :all)))

                    (mirror-p-els axis pos))))))
     els-parts)))

;; --- traverse (HOF)

(defn update-p-el-xys
  [el f & args]
  (-> el (update :el-argv #(mapv f %))))

(defn update-p-els
  [el f & args]
  (-> el
      (update :el-argv #(apply f % args))))

(defn map-p-xys [f p-els]
  (->> p-els
       (mapv (fn [{:as p-el :keys [el-k]}]
               (let [el' (case el-k
                           :A      (-> p-el (update-in [:el-argv 2] f))
                           :circle (-> p-el (update-in [:el-opts :center] f))
                           (-> p-el
                               (update-p-el-xys f)))]
                 (-> el' (with-meta (meta p-el))))))))


(defn update-s-els [cmpt f & args]
  (-> cmpt
      (update :defs   (partial u/map-vals #(apply f % args)))
      (update :script (partial mapv       #(apply update-p-els % f args)))))

(defn update-s-el-sel [cmpt {:as navr-sel :keys [src-k]} f & args]
  (if-not navr-sel
    (apply update-s-els cmpt f args)
    (case src-k
      :defs   (apply nav/update-in-nav* cmpt navr-sel :x-el-k f args)
      :script (apply nav/update-in-nav* cmpt navr-sel :x-el-k update-p-els f args))))

;; --- scale

(defn scale-p-els [els center factor]
  (map-p-xys #(u/tl-point-towards % center factor) els))

;; --- translate

(defn translate-p-els [els xyd]
  (map-p-xys (partial u/v+ xyd) els))

;; --- rotate

(defn rotate-p-els [els ctr alpha]
  (map-p-xys #(u/tl-point-around ctr % alpha) els))

;; --- meta

(defn- with-xy-abs-meta [xy-rel
                         xy-abs] (-> xy-rel
                                     (with-meta {:xy-abs xy-abs})))

(defn xy-abs-meta [xy-rel] (-> xy-rel meta :xy-abs))

(defn attach-xy-abs-meta [p-els]
  (map (fn [{:as p-el :keys [p-el-k], xys  :el-argv}
            {:as p-el-norm,           xys' :el-argv}]
         (let [p-el' (case p-el-k
                       :c (-> p-el
                              (assoc :el-argv (map with-xy-abs-meta xys xys')))
                       :s (let [[c2  tgt]  xys
                                [c2' tgt'] xys']
                            (-> p-el
                                (assoc :el-argv [(with-xy-abs-meta c2 c2')
                                                 (with-xy-abs-meta tgt tgt')])))
                       :t (let [[tgt]  xys
                                [tgt'] xys']
                            (-> p-el
                                (assoc :el-argv [(with-xy-abs-meta tgt tgt')])))
                       p-el)]
           (-> p-el'
               (with-meta (meta p-el)))))
       p-els
       (-> p-els (normalize-pcmd-seq :op :rel->abs))))

(defn- attach-nav-rec-meta
  ;; TODO: remove (obsolete)
  [navr p-els]
  (->> p-els
       (map-indexed (fn [i p-el]
                      (-> p-el
                          (with-meta {:nav-rec
                                      (-> navr
                                          (assoc :p-el-i i))}))))
       vec))

(defn attach-nav-rec-meta*
  ;; TODO: remove (obsolete)
  [script navr-sel]
  (->> script
       (map-indexed
        (fn [s-el-i
             {:as s-el :keys [el-k]}]
          (let [nav-rec (-> (or navr-sel
                                (nav/nav-rec))
                            (assoc :src-k  :script
                                   :x-el-k s-el-i))]
            (case el-k
              :path (-> s-el
                        (update-p-els (fn [p-els]
                                        (attach-nav-rec-meta nav-rec p-els))))
              s-el))))
       vec))

(defn p-el->pnts [els]
  (map (fn [el]
         [el (data/parse-pcmd-pnts el)])
       els))

(defn resolve-els-ref  [defs ref] (get    defs (get-in ref [:el-argv 0])))
(defn resolve-d-ref    [defs ref] (get-in defs [:d (get-in ref [:el-argv 0])]))
(defn resolve-cmpt-ref [defs ref] (get-in defs [:components (get-in ref [:el-argv 0])]))

(defn resolve-els-refs
  [defs els]
  (->> els
       (into []
             (mapcat (fn [elr]
                       (if (el/ref? elr)
                         (let [navr (nav/nav-rec :src-k  :defs
                                                 :x-el-k (get-in elr [:el-argv 0]))]
                           (->> (resolve-els-ref defs elr)
                                (attach-nav-rec-meta navr)))
                         [elr]))))))

(defn get-path-segment
  [src-k-sel els eli]
  (let [el-prev (nav/els-prev els (case src-k-sel :defs :eln :eli) eli)
        el      (nav/els>     els (case src-k-sel :defs :eln :eli) eli)]
    (vec
     (concat
      (when (and el-prev
                 (not= :M (:el-k el)))
        (list (-> el-prev
                  (assoc :el-k    :M
                         :el-argv [(-> el-prev :el-argv peek)]))))
      (list el)))))

;; --- path opts

(declare apply-path-opts)

(defn- apply-repeat
  [els cmpt-ctx
   {:as opts
    {repeat-times :times
     repeat-mode  :mode} :repeat}]
  (let [opts' (-> opts
                  (dissoc :repeat)
                  (merge (:repeat opts)))

        [els-head
         els-init] (if (= :fuse repeat-mode)
                     [(take 1 els)
                      (rest els)]
                     [nil els])]

    (->> (range 0 repeat-times)
         (reduce (fn [[els-prev :as acc] _i]
                   (let [els+ (->> els-prev
                                   (apply-path-opts cmpt-ctx opts'))]
                     (conj acc els+)))
                 (list els-init))
         reverse
         (apply concat els-head)
         (#(cond-> %
                   (= :fuse repeat-mode)
                   (-> drop-last
                       (concat [(data/elem :el-k :z)])))))))

(defn apply-path-opts
  [{:as cmpt-ctx :keys [interactive? coords?], cmpt-defs :defs}
   {:as opts
    :keys [close? width mirror repeat]

    {:as scale
     scale-ctr    :center
     scale-factor :factor} :scale

    {:as rotate
     rot-ctr :center
     rot-deg :degree} :rotate

    translate :translate

    {mirror-mode  :mode
     mirror-pos   :pos
     mirror-axis  :axis
     :or {mirror-pos 100
          mirror-axis  0}} :mirror}
   pcmd-seq]
  (-> pcmd-seq
      (->> (resolve-els-refs cmpt-defs))
      (cond-> scale     (scale-p-els scale-ctr scale-factor)
              translate (translate-p-els translate)
              rotate    (rotate-p-els rot-ctr rot-deg)
              close?    (concat [(data/elem :el-k :z)])
              (and repeat
                   (not interactive?)) (apply-repeat cmpt-ctx opts)

              (and mirror
                   (not coords?))
              (->> (add-mirrored mirror-mode
                                 mirror-axis
                                 mirror-pos)))))
