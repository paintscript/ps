(ns paintscript.els
  (:require [paintscript.util :as u]
            [paintscript.el-path :as el]
            [paintscript.nav :as nav]))

;; -----------------------------------------------------------------------------
;; normalize

(defn normalize-p-els [els & {:keys [op] :or {op :all}}]
  {:pre [(when op (-> op #{:all :rel->abs :short->full}))]}
  (loop [[[p-el-k :as el] & els-tail] els
         tgt-prev nil
         cp-prev  nil
         acc      []]
    (if-not p-el-k
      acc
      (let [[el' tgt' cp']
            (reduce (fn [[el _tgt-prev _cp-prev] f-step]
                      (f-step el tgt-prev cp-prev))
                    [el tgt-prev cp-prev]
                    (el/normalization-steps p-el-k op))
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
  (loop [[el & els-tail] els
         acc []
         tgt-prev nil]
    (if-not el
      (-> (reverse acc)
          (cond-> (not drop-last?)
                  (conj [:M tgt-prev])))
      (let [[el' tgt] (el/el->reversed el tgt-prev)]
        (recur els-tail (-> acc (cond-> el' (conj el'))) tgt)))))

(defn reverse-p-els
  ([els] (reverse-p-els nil els))
  ([opts els]
   (->> els
        normalize-p-els
        (reverse-p-el-xys opts))))

;; --- mirror

(defn- mirror-xy  [axis pos pnt] (update pnt axis #(- pos %)))
(defn- mirror-xys [axis pos xys] (map #(mirror-xy axis pos %) xys))

(defn mirror-p-els [axis pos els]
  (->> els
       (mapv (fn [[p-el-k & xys :as el]]
               (case p-el-k
                 :arc (let [[opts & xys] xys]
                        (concat [p-el-k (-> opts
                                          (update :mode
                                                  #(case % :convex :concave :convex)))]
                                (mirror-xys axis pos xys)))
                 :A (let [[r [rot f1 f2] xy] xys

                          ;; NOTE: needs to be undone when combined with reverse
                          f2' (-> f2 el/flip-bin)]
                      [:A r [rot f1 f2'] (mirror-xy axis pos xy)])
                 (vec
                  (cons p-el-k
                        (mirror-xys axis pos xys))))))))

(defn add-mirrored
  "subdivide els in chunks terminated w/ :z and append a mirrored version after each"
  [mode axis pos els]
  (let [els-parts
        (as-> (partition-by #{[:z] [:Z]} els) coll
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
                                     (normalize-p-els
                                      ;; - :rel->abs for math
                                      ;; - :short->full for uniform data (esp. :arc)
                                      :op :all)))
                    (mirror-p-els axis pos))))))
     els-parts)))

;; --- traverse (HOF)

(defn update-p-el-xys
  [[p-el-k & xys :as p-el] f & args]
  (vec
   (cons p-el-k
         (map f xys))))

(defn map-p-xys [f p-els]
  (->> p-els
       (mapv (fn [[p-el-k & xys :as p-el]]
               (let [el' (case p-el-k
                           :A      (-> p-el (update 3 f))
                           :circle (-> p-el (update-in [1 :center] f))
                           (-> p-el
                               (update-p-el-xys f)))]
                 (-> el' (with-meta (meta p-el))))))))

(defn update-p-els
  [s-el f & args]
  {:pre [(-> s-el (get 0) (= :path))
         (-> s-el (get 1) map?)]}
  (let [[init
         tail] (split-at 2 s-el)]
    (vec
     (concat init
             (apply f tail args)))))

(defn extract-p-els
  [s-el src-k-sel p-eli-sel]
  {:pre [(-> s-el (get 0) (= :path))
         (-> s-el (get 1) map?)]}
  (->> s-el
       (take (inc p-eli-sel))
       (drop (case src-k-sel
               :defs 0
               nav/p-el-i0))))

(defn update-s-els [cmpt f & args]
  (-> cmpt
      (update :defs   (partial u/map-vals #(apply f % args)))
      (update :script (partial mapv       #(apply update-p-els % f args)))))

(defn update-s-el-sel [cmpt {:as sel-rec :keys [src-k]} f & args]
  (if-not sel-rec
    (apply update-s-els cmpt f args)
    (case src-k
      :defs   (apply nav/update-in-pth* cmpt sel-rec :x-el-k f args)
      :script (apply nav/update-in-pth* cmpt sel-rec :x-el-k update-p-els f args))))

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

(defn- with-xy-abs-meta [xy1 xy2] (with-meta xy1 {:xy-abs xy2}))
(defn xy-abs-meta [xy] (-> xy meta :xy-abs))

(defn attach-xy-abs-meta [p-els]
  (map (fn [[p-el-k   & xys :as p-el]
            [_p-el-k' & xys' :as p-el-norm]]
         (let [p-el' (case p-el-k
                       :c (vec (cons p-el-k
                                     (map with-xy-abs-meta xys xys')))
                       :s (let [[c2  tgt]  xys
                                [c2' tgt'] xys']
                            [p-el-k
                             (with-xy-abs-meta c2 c2')
                             (with-xy-abs-meta tgt tgt')])
                       :t (let [[tgt]  xys
                                [tgt'] xys']
                            [p-el-k
                             (with-xy-abs-meta tgt tgt')])
                       p-el)]
           (-> p-el'
               (with-meta (meta p-el)))))
       p-els
       (-> p-els (normalize-p-els :op :rel->abs))))

(defn- attach-pth-rec-meta
  [pth-rec p-el-i0 p-els]
  (->> p-els
       (map-indexed (fn [i p-el]
                      (-> p-el
                          (with-meta {:pth-rec
                                      (-> pth-rec
                                          (assoc :p-el-i (+ p-el-i0 i)))}))))
       vec))

(defn attach-pth-rec-meta*
  [script sel-rec]
  (->> script
       (map-indexed
        (fn [s-el-i
             [s-el-k :as s-el]]
          (let [pth-rec (-> (or sel-rec
                                (nav/pth-rec))
                            (assoc :src-k  :script
                                   :x-el-k s-el-i))]
            (case s-el-k
              :path (-> s-el
                        (update-p-els (fn [p-els]
                                        (attach-pth-rec-meta pth-rec nav/p-el-i0 p-els))))
              s-el))))
       vec))

;; TODO: rename to el-k, also use for s-el
(defrecord El      [p-el-k opts i-arg0 args])
(defrecord MainPnt [xy xy-abs pth-rec])
(defrecord CtrlPnt [xy xy-abs pth-rec i-main])

(defn el-vec-->el-rec
  [[p-el-k & [arg1 :as args] :as p-el]]
  (-> (if (map? arg1)
        (->El p-el-k arg1 2 (rest args))
        (->El p-el-k nil  1 args))
      (with-meta (meta p-el))))

(defn- derive-p-el-pnts
  [{:as el-rec :keys [p-el-k i-arg0 args]}
   p-el-i]
  (let [{:keys
         [pth-rec]} (meta el-rec)
        cp-cnt      (dec (count args))]
    (map-indexed (fn [xy-i xy]
                   (let [pth-rec' (-> pth-rec
                                      (assoc :xy-i (+ i-arg0 xy-i)))]
                     ;; TODO: handle arc flags and shape commands
                     ;; NOTE: CPs always come first, the main point last
                     (if (and (el/has-cp? p-el-k)
                              (< xy-i cp-cnt))
                       (let [i-main (if (and (= 2 cp-cnt)
                                             (= 0 xy-i))
                                      (dec p-el-i)
                                      p-el-i)]
                         (->CtrlPnt xy (-> xy meta :xy-abs) pth-rec' i-main))
                       (->MainPnt xy (-> xy meta :xy-abs) pth-rec'))))
                 args)))

(defn p-el->pnts [p-el-recs]
  (map-indexed (fn [p-el-i
                    p-el-rec]
                 [p-el-rec (derive-p-el-pnts p-el-rec p-el-i)])
               p-el-recs))

(defn get-opts [el]
  (let [v (get el 1)]
    (when (map? v) v)))

(defn resolve-els-ref  [defs ref] (get    defs (get ref 1)))
(defn resolve-d-ref    [defs ref] (get-in defs [:d (get ref 1)]))
(defn resolve-cmpt-ref [defs ref] (get-in defs [:components (last ref)]))

(defn resolve-els-refs
  [defs els]
  (->> els
       (mapcat (fn [el]
                 (if (el/ref? el)
                   (let [pth-rec (nav/pth-rec :src-k  :defs
                                              :x-el-k (get el 1))]
                     (->> (resolve-els-ref defs el)
                          (attach-pth-rec-meta pth-rec 0)))
                   [el])))))

(defn get-path-segment [src-k-sel els eli]
  (let [el-prev    (nav/els-prev els (case src-k-sel :defs :eln :eli) eli)
        [k :as el] (nav/els>     els (case src-k-sel :defs :eln :eli) eli)]
    (concat
     (when (and el-prev
                (not= :M (first el)))
       (list [:M (last el-prev)]))
     (list el))))

;; --- path opts

(declare apply-path-opts)

(defn- apply-repeat
  [els cmpt
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
                                   (apply-path-opts cmpt opts'))]
                     (conj acc els+)))
                 (list els-init))
         reverse
         (apply concat els-head)
         (#(cond-> %
                   (= :fuse repeat-mode)
                   (-> drop-last
                       (concat [[:z]])))))))

(defn apply-path-opts
  [{:as cmpt :keys [defs interactive? coords?]}
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
   els]
  (-> els
      (->> (resolve-els-refs defs))
      (cond-> scale     (scale-p-els scale-ctr scale-factor)
              translate (translate-p-els translate)
              rotate    (rotate-p-els rot-ctr rot-deg)
              close?    (concat [[:z]])
              (and repeat
                   (not interactive?)) (apply-repeat cmpt opts)

              (and mirror
                   (not coords?))
              (->> (add-mirrored mirror-mode
                                 mirror-axis
                                 mirror-pos)))))
