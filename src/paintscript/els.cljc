(ns paintscript.els
  (:require [paintscript.util :as u]
            [paintscript.el :as el]
            [paintscript.nav :as nav]))

;; -----------------------------------------------------------------------------
;; normalize

(defn normalize-els [els & {:keys [op] :or {op :all}}]
  (loop [[[el-k :as el] & els-tail] els
         tgt-prev nil
         cp-prev  nil
         acc      []]
    (if-not el-k
      acc
      (let [[el' tgt' cp']
            (reduce (fn [[el tgt-prev cp-prev] f-step]
                      (f-step el tgt-prev cp-prev))
                    [el tgt-prev cp-prev]
                    (el/normalization-steps el-k op))
            el'' (with-meta el' (meta el))]
        (recur els-tail tgt' cp' (conj acc el''))))))

;; -----------------------------------------------------------------------------
;; transform

;; --- reverse

(defn- reverse-el-xys
  "drop last point (implicit) and redistribute the rest in reverse:
    ([:M 1] [:L 2 3] [:C 4 5 6] [:C 7 8 9])
    ~> ([:C 7 8 9] [:C 4 5 6] [:L 2 3] [:M 1])
    => ([:C 8 7 6] [:C 5 4 3] [:L 2 1])
  "
  [els]
  (loop [[el & els-tail] els
         acc []
         tgt-prev nil]
    (if-not el
      (reverse acc)
      (let [[el' tgt] (el/el->reversed el tgt-prev)]
        (recur els-tail (-> acc (cond-> el' (conj el'))) tgt)))))

(defn- reverse-els
  [width els]
  (->> els
       normalize-els
       reverse-el-xys))

;; --- mirror

(defn- mirror-xy  [axis pos pnt] (update pnt axis #(- pos %)))
(defn- mirror-xys [axis pos xys] (map #(mirror-xy axis pos %) xys))

(defn mirror-els [axis pos els]
  (->> els
       (mapv (fn [[el-k & xys :as el]]
               (case el-k
                 :arc (let [[opts & xys] xys]
                        (concat [el-k (-> opts
                                          (update :mode
                                                  #(case % :convex :concave :convex)))]
                                (mirror-xys axis pos xys)))
                 :A (let [[r [rot f1 f2] xy] xys

                          ;; NOTE: needs to be undone when combined with reverse
                          f2' (-> f2 el/flip-bin)]
                      [:A r [rot f1 f2'] (mirror-xy axis pos xy)])
                 (vec
                  (cons el-k
                        (mirror-xys axis pos xys))))))))

(defn mirror-els* [mode axis pos els]
  (let [els-parts (as-> (partition-by #{[:z]} els) coll
                        (if (odd? (count coll))
                          (-> coll (concat [nil]))
                          coll)
                        (->> coll
                             (partition 2)
                             (map #(apply concat %))))]
    (sequence
     (mapcat
      (fn [els-part]
        (concat
         els-part
         (->> (case mode
                :merged   (->> els-part (reverse-els pos))
                :separate (->> els-part normalize-els))
              (mirror-els axis pos)))))
     els-parts)))

;; --- traverse (HOF)

(defn map-xys [f els]
  (->> els
       (mapv (fn [[el-k & xys :as el]]
               (let [el' (case el-k
                           :A (-> el (update 3 f))
                           (vec
                            (cons el-k
                                  (map f xys))))]
                 (-> el' (with-meta (meta el))))))))

(defn update-p-els [p f & args]
  (vec
   (concat (take 2 p)
           (apply f (drop 2 p) args))))

(defn update-px-all [params f & args]
  (-> params
      (update :defs   (fn [dd] (u/map-vals #(apply f % args) dd)))
      (update :script (fn [s]  (mapv #(apply update-p-els % f args) s)))))

(defn update-px [params [src-k px :as sel] f & args]
  (if-not sel
    (apply update-px-all params f args)
    (case src-k
      :defs   (apply update-in params [src-k px] f args)
      :script (apply update-in params [src-k px] update-p-els f args))))

;; --- scale

(defn scale-els [els center factor]
  (map-xys #(u/tl-point-towards % center factor) els))

;; --- translate

(defn translate-els [els xyd]
  (map-xys (partial u/v+ xyd) els))

;; --- meta

(defn- with-xy-abs-meta [xy1 xy2] (with-meta xy1 {:xy-abs xy2}))
(defn xy-abs-meta [xy] (-> xy meta :xy-abs))

(defn attach-xy-abs-meta [els]
  (let [els' (-> els normalize-els)]
    (map (fn [[el-k   & xys :as el]
              [_el-k' & xys']]
           (let [el' (case el-k
                       :c (vec (cons el-k
                                     (map with-xy-abs-meta xys xys')))
                       :s (let [[  c2  tgt]  xys
                                [_ c2' tgt'] xys']
                            [el-k
                             (with-xy-abs-meta c2 c2')
                             (with-xy-abs-meta tgt tgt')])
                       el)]
             (-> el'
                 (with-meta (meta el)))))
         els
         els')))

(defn- attach-ii-el-meta
  [src-k x-k eli0 els]
  (->> els
       (map-indexed (fn [eli el]
                      (with-meta el {:ii-el [src-k x-k (+ eli0 eli)]})))
       vec))

(defn attach-ii-el-meta* [script-pp]
  (->> script-pp
       (map-indexed (fn [pi p]
                      (vec
                       (concat (take 2 p)
                               (->> (drop 2 p)
                                    (attach-ii-el-meta :script pi nav/eli0))))))
       vec))

(defrecord El      [el-k opts ii-el i-arg0 args])
(defrecord MainPnt [xy xy-abs ii-pnt])
(defrecord CtrlPnt [xy xy-abs ii-pnt i-main])

(defn args-->pnts
  [{:as el :keys [el-k i-arg0 args]}
   eli
   {:as el-meta :keys [ii-el xy-abs]}]
  (let [cp-cnt (dec (count args))]
    (map-indexed (fn [i-xy xy]
                   (let [ii-pnt (concat ii-el [(+ i-arg0 i-xy)])]
                     (if (and (el/has-cp? el-k)
                              (< i-xy cp-cnt))
                       (let [i-main (if (and (= 2 cp-cnt)
                                             (= 0 i-xy))
                                      (dec eli)
                                      eli)]
                         (->CtrlPnt xy xy-abs ii-pnt i-main))
                       (->MainPnt xy xy-abs ii-pnt))))
                 args)))

(defn el-pnts [el-recs]
  (map-indexed (fn [eli el]
                 [el (args-->pnts el eli (meta el))])
               el-recs))

(defn resolve-els-ref [defs ref] (get    defs (get ref 1)))
(defn resolve-d-ref   [defs ref] (get-in defs [:d (get ref 1)]))

(defn resolve-els-refs
  [defs els]
  (->> els
       (mapcat (fn [el]
                 (if (el/ref? el)
                   (->> (resolve-els-ref defs el)
                        (attach-ii-el-meta :defs (get el 1) 0))
                   [el])))))

(defn get-path-segment [src-k-sel els eli]
  (let [el-prev    (nav/els-prev els (case src-k-sel :defs :eln :eli) eli)
        [k :as el] (nav/els>     els (case src-k-sel :defs :eln :eli) eli)]
    (concat
     (when (and el-prev
                (not= :M (first el)))
       (list [:M (last el-prev)]))
     (list el))))

(defn el-vec-->el-rec
  ([elv] (el-vec-->el-rec elv nil))
  ([[el-k & [arg1 :as args] :as el-vec] ii-el]
   (with-meta (if (map? arg1)
                (->El el-k arg1 ii-el 2 (rest args))
                (->El el-k nil  ii-el 1 args))
     (meta el-vec))))

;; --- path opts

(defn apply-path-opts
  [{:as params :keys [defs debug? coords?]}
   {:as opts
    :keys [close? width mirror]
    {scale-ctr    :center
     scale-factor :factor :as scale} :scale
    translate :translate
    {mirror-mode  :mode
     mirror-pos   :pos
     mirror-axis  :axis
     :or {mirror-pos 100
          mirror-axis  0}} :mirror}
   els]
  (-> els
      (->> (resolve-els-refs defs))
      (cond-> scale     (scale-els scale-ctr scale-factor)
              translate (translate-els translate)
              close?    (concat [[:z]])

              (and mirror
                   (not coords?))
              (->> (mirror-els* mirror-mode
                                mirror-axis
                                mirror-pos)))))
