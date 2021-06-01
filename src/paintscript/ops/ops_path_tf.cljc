(ns paintscript.ops.ops-path-tf
  (:require [paintscript.util :as u]
            [paintscript.nav :as nav]
            [paintscript.data :as data]
            [paintscript.ops.ops-elem :as ops-elem]
            [paintscript.ops.ops-path :as ops-path]))

(defn map-pcmd-xys [f pcmd-seq]
  (->> pcmd-seq
       (mapv (fn [{:as p-el :keys [el-k]}]
               (let [el' (case el-k
                           :A      (-> p-el (update-in [:el-argv 2] f))
                           :circle (-> p-el (update-in [:el-opts :center] f))
                           (-> p-el
                               (ops-elem/map-el-args f)))]
                 (-> el' (with-meta (meta p-el))))))))

(defn pcmds-xys [pcmd-seq]
  (->> pcmd-seq
       (map #(case (:el-k %)
               :A      (-> % :el-argv (get 2))
               :circle (-> % :el-opts :center)
               (-> % :el-argv peek)))))

(defn normalize-pcmds
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
                          (update 0 ops-path/merge-el-tup el)))
                    [el tgt-prev cp-prev]
                    (ops-path/normalization-steps el-k op))
            el'' (-> el' (with-meta (meta el)))]
        (recur els-tail tgt' cp' (conj acc el''))))))

(defn- reverse-pcmds*
  "drop last point (implicit) and redistribute the rest in reverse:
    ([:M 1] [:L 2 3] [:C 4 5 6] [:C 7 8 9])
    ~> ([:C 7 8 9] [:C 4 5 6] [:L 2 3] [:M 1])
    => ([:C 8 7 6] [:C 5 4 3] [:L 2 1])
  "
  [{:keys [drop-last?]} pcmd-seq]
  ;; TODO: handle (:Z :z)
  (loop [[pcmd & pcmds-tail] pcmd-seq
         acc      []
         tgt-prev nil]
    (if-not pcmd
      (-> (reverse acc)
          (cond-> (not drop-last?)
                  (conj (data/elemv :M [tgt-prev]))))
      (let [[pcmd' tgt] (ops-path/el->reversed pcmd tgt-prev)]
        (recur pcmds-tail (-> acc (cond-> pcmd' (conj pcmd'))) tgt)))))

(defn reverse-pcmds
  ([els] (reverse-pcmds nil els))
  ([opts els]
   (->> els
        normalize-pcmds
        (reverse-pcmds* opts))))

;; --- mirror

(defn- mirror-xy  [axis pos pnt] (update pnt axis #(- pos %)))
(defn- mirror-xys [axis pos xys] (map #(mirror-xy axis pos %) xys))

(defn mirror-pcmds [pcmd-seq axis pos]
  (->> pcmd-seq
       (mapv (fn [{:as el :keys [el-k el-opts], xys :el-argv}]
               (case el-k
                 :arc (-> el
                          (update-in [:el-opts :mode]
                                     #(case % :convex :concave :convex))
                          (update :el-argv
                                  #(mirror-xys axis pos %)))
                 :A (let [[r [rot f1 f2] xy] xys

                          ;; NOTE: needs to be undone when combined with reverse
                          f2' (-> f2 ops-path/flip-bin)]
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
               (-> (case mode
                     :merged   (->> els-part (reverse-pcmds {:drop-last? true}))
                     :separate (->  els-part
                                    (normalize-pcmds
                                     ;; - :rel->abs for math
                                     ;; - :short->full for uniform data (esp. :arc)
                                     :op :all)))

                   (mirror-pcmds axis pos))))))
     els-parts)))

(defn min* [a b] (if a (min a b) b))
(defn max* [a b] (if a (max a b) b))

(defn pcmds-cxy
  "derive the center of all target-xys by deriving the mins & maxes
   and then halving the delta"
  [pcmds-seq]
  (let [xys (pcmds-xys pcmds-seq)
        acc (reduce (fn [acc [x y]]
                                  (-> acc
                                      (update :x-min min* x)
                                      (update :x-max max* x)
                                      (update :y-min min* y)
                                      (update :y-max max* y)))
                                nil
                                xys)]
    (mapv (fn [a b]
            (-> b (- a) (/ 2)))
          (mapv acc [:x-min :y-min])
          (mapv acc [:x-max :y-max]))))

(defn scale-pcmds [pcmd-seq center factor]
  (let [cxy (or center
                (pcmds-cxy pcmd-seq))]
    (->> pcmd-seq
         (map-pcmd-xys #(u/tl-point-towards % cxy factor)))))

(defn translate-pcmds [pcmd-seq xy-delta]
  (->> pcmd-seq
       (map-pcmd-xys (partial u/v+ xy-delta))))

(defn rotate-pcmds [pcmd-seq ctr alpha]
  (->> pcmd-seq
       (map-pcmd-xys #(u/tl-point-around ctr % alpha))))

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
      (->> (ops-elem/resolve-els-refs cmpt-defs))
      (cond-> scale     (scale-pcmds scale-ctr scale-factor)
              translate (translate-pcmds translate)
              rotate    (rotate-pcmds rot-ctr rot-deg)
              close?    (concat [(data/elem :el-k :z)])
              (and repeat
                   (not interactive?)) (apply-repeat cmpt-ctx opts)

              (and mirror
                   (not coords?))
              (->> (add-mirrored mirror-mode
                                 mirror-axis
                                 mirror-pos)))))

;; -----------------------------------------------------------------------------
;; extract

(defn p-el->pnts [els]
  (map (fn [el]
         [el (data/parse-pcmd-pnts el)])
       els))

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


;; -----------------------------------------------------------------------------
;; add/delete/transform

(defn has-pred? [pi] (pos? pi))

(defn el-len  [el] (-> el :el-argv count))
(defn get-tgt [el] (-> el :el-argv peek))

(defn offset-pnt [pnt delta] (mapv + pnt delta))
(defn xy-delta   [pnt1 pnt2] (mapv - pnt1 pnt2))

(defn get-pnt [els [eli xyi-abs]]
  (get-in els [eli :el-argv (or xyi-abs 0)]))

(defn get-pred-tgt [els eli]
  (-> (get els (-> eli (- 1))) get-tgt))

(defn tail-iii [{:as cmpt :keys [script]}]
  (let [pi  (-> script count (- 1) (max 0))
        eli (-> script (get pi) count (- 1) (max 1))
        xyi (some-> script (get-in [pi eli]) count (- 1))]
    [:script pi eli xyi]))

;; --- points

; (def xyi-offset 1)
; (def el-i-offset 2)

(defn infer-succ-pnt
  [els eli xyi]
  (let [el    (get els eli)
        xyi'  (+ xyi
                 ; xyi-offset
                 )
        pnt-0 (get-pnt els [eli xyi'])]
    (case (el-len el)
      0 (if (has-pred? eli)
          (offset-pnt (get-pred-tgt els eli)
                      [10 10])
          [0 0])

      1 (if (has-pred? eli)
          (let [pnt-1 (-> (get els (-> eli (- 1))) get-tgt)]
            (offset-pnt pnt-0 (xy-delta pnt-0 pnt-1)))
          (offset-pnt pnt-0 (xy-delta pnt-0 [0 0])))

      (let [pnt-1 (get-pnt els [eli (-> xyi' (- 1))])]
        (offset-pnt pnt-0
                    (xy-delta pnt-0 pnt-1))))))

(defn append-pnt
  ([els eli]
   (append-pnt els eli (-> (get els eli) el-len dec)))

  ;; infer successor
  ([els eli xyi]
   (append-pnt els eli xyi
               (infer-succ-pnt els eli xyi)))

  ;; explicit:
  ([els eli xyi pnt]
   (let [xyi' (or xyi 0
                 ; xyi-offset
                 )]
     (-> els
         (update-in [eli :el-argv] u/vec-append xyi' pnt)))))

(defn del-pnt
  [els eli xyi]
  (let [xyi' (+ xyi
                ; xyi-offset
                )]
    (-> els
        (update-in [eli :el-argv] u/vec-remove xyi'))))

;; --- els

(defn derive-successor-pcmd [els eli]
  (let [{:as el-1 :keys [el-k], pnts :el-argv} (get els eli)]
    (case el-k
      :M (data/elem :el-k :L, :el-argv [(offset-pnt (last pnts) [10 10])])
      :L (data/elem :el-k :L, :el-argv [(offset-pnt (last pnts) [10 10])])
      :C (let [tgt (offset-pnt (last pnts) [10 10])]
           (data/elem :el-k :S, :el-argv [(last pnts) tgt]))
      :S (let [tgt (offset-pnt (last pnts) [10 10])]
           (data/elem :el-k :S, :el-argv [tgt tgt])))))

(defn append-p-el
  ([p-els p-el-i] (let [p-el (derive-successor-pcmd p-els p-el-i)]
                      (append-p-el p-els p-el-i p-el)))
  ([p-els p-el-i p-el]
   (cond
     (and (record? p-els)
          (-> p-els :el-k (= :path)))
     (let [p-el-i  (or p-el-i
                       (-> p-els :el-argv count dec))]
       (-> p-els
           (update :el-argv u/vec-append p-el-i p-el)

           ;; NOTE: when a pcmd is inserted anywhere other than at the end,
           ;; all subsequent locrs become out of sync; a conversion round-trip
           ;; re-initializes them correctly
           data/refresh-elrr))

     ;; --- path-seq (:defs)
     (seq p-els) (-> p-els (u/vec-append p-el-i p-el))
     :else       [p-el])))

(defn del-el
  [els eli]
  (-> els
      (u/vec-remove eli)))

(defn p-el-prev [pth p-el-i]
  (let [v (get pth (dec p-el-i))]
    (when (record? v)
      v)))

(defn change-pcmd-k*
  [pth-el-argv p-el-i to]
  (let [{:as el,   p-el-k :el-k} (get pth-el-argv p-el-i)
        {:as el-1, el-1-k :el-k} (p-el-prev pth-el-argv p-el-i)]
    (case p-el-k
      :L (let [tgt (-> el :el-argv peek)]
           (case to
             :S (data/elemv :S [tgt tgt])
             :Q (data/elemv :Q [tgt tgt])
             :C (data/elemv :C [(-> el-1 :el-argv peek) tgt tgt])))
      :C (let [{[c1 c2 tgt] :el-argv} el]
           (case to
             :S (data/elemv :S [c2 tgt])
             :Q (data/elemv :Q [c2 tgt])
             :L (data/elemv :L [tgt])))
      :S (let [{[c tgt] :el-argv} el]
           (case to
             :L (data/elemv :L [tgt])
             :Q (data/elemv :Q [c tgt])
             :C (data/elemv :C [(-> el-1 :el-argv peek) c tgt])))
      :Q (let [{[c tgt] :el-argv} el]
           (case to
             :L (data/elemv :L [tgt])
             :S (data/elemv :S [c tgt])
             :C (data/elemv :C [(-> el-1 :el-argv peek) c tgt]))))))

(defn change-pcmd-k
  [pth p-el-i to]
  (cond
    (record? pth) (let [pth'  (-> pth
                                  (update :el-argv normalize-pcmds :op :rel->abs))
                        p-el' (change-pcmd-k* (:el-argv pth) p-el-i to)]
                    (-> pth
                        (update :el-argv u/vec-replace p-el-i p-el')
                        data/refresh-elrr))

    ;; path-seq (only used in ops-xspace?)
    (vector? pth) (let [pth   (-> pth
                                  (normalize-pcmds :op :rel->abs))
                        p-el' (change-pcmd-k* pth p-el-i to)]
                    (-> pth
                        (u/vec-replace p-el-i p-el')))))
