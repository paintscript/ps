(ns paintscript.pth-vecs
  (:require [paintscript.util :as u]
            [paintscript.pth-vec :as pv]))

(def i-pth-vec0 2)
(def i-pnt0 1)

(defn- arcs
  [arc-pnts {:as opts :keys [mode ctd?] :or {mode :concave}}]
  (let [[head & tail] arc-pnts
        paired-with-prev (map vector tail (drop-last arc-pnts))
        arc-middle (str (case mode :concave "0,1" :convex "0,0") )]
    (concat (when-not ctd?
              [["M" head]])
            (for [[[x  y  :as pnt]
                   [xp yp :as pnt-prev]] paired-with-prev]
              ["A" [(- x xp) (- y yp)] [0 arc-middle] [x y]]))))

(defn- parse-vec [[k & args]]
  (if (map? (first args))
    [k (first args) 2 (rest args)]
    [k nil 1 args]))

(defn- mirror-pnt  [width pnt]  (update pnt 0 #(- width %)))
(defn- mirror-pnts [width pnts] (map #(mirror-pnt width %) pnts))

(defn- mirror-pth-vecs [width pth-vv]
  (->> pth-vv
       (map (fn [[pth-k & pnts :as pth-v]]
              (case pth-k
                :arc (let [[opts & pnts] pnts]
                       (concat [pth-k (-> opts
                                          (update :mode
                                                  #(case % :convex :concave :convex)))]
                               (mirror-pnts width pnts)))
                :A (let [[r [rot f1 f2] xy] pnts

                               ;; NOTE: needs to be undone when combined with reverse
                               f2' (-> f2 pv/flip-bin)]
                           [:A r [rot f1 f2'] (mirror-pnt width xy)])
                (cons pth-k
                      (mirror-pnts width pnts)))))))

(defn normalize-path-vecs [pvv]
  (loop [[[pv-k :as pv] & pvv-tail] pvv
         tgt-prev nil
         cp-prev  nil
         acc      []]
    (if-not pv-k
      acc
      (let [[pv' tgt' cp']
            (reduce (fn [[pv tgt-prev cp-prev] f-step]
                      (f-step pv tgt-prev cp-prev))
                    [pv tgt-prev cp-prev]
                    (pv/normalization-steps pv-k))]
        (recur pvv-tail tgt' cp' (conj acc pv'))))))

(defn- with-abs-meta [pnt1 pnt2] (with-meta pnt1 {:abs pnt2}))
(defn abs-meta [pnt] (-> pnt meta :abs))

(defn attach-normalized-meta [pth-vecs pth-vecs']
  (map (fn [[k & pnts :as pth-vec] [k' & pnts']]
         (case k
           :c (vec (cons k (map with-abs-meta pnts pnts')))
           :s (let [[  c2  tgt]  pnts
                    [_ c2' tgt'] pnts']
                [k (with-abs-meta c2 c2') (with-abs-meta tgt tgt')])
           pth-vec))
       pth-vecs
       pth-vecs'))

(defn- reverse-pth-vec-pnts
  "drop last point (implicit) and redistribute the rest in reverse:
    ([:M 1] [:L 2 3] [:C 4 5 6] [:C 7 8 9])
    ~> ([:C 7 8 9] [:C 4 5 6] [:L 2 3] [:M 1])
    => ([:C 8 7 6] [:C 5 4 3] [:L 2 1])
  "
  [pvv]
  (loop [[pv & pvv-tail] pvv
         acc []
         tgt-prev nil]
    (if-not pv
      (reverse acc)
      (let [[pv' tgt] (pv/pv->reversed pv tgt-prev)]
        (recur pvv-tail (-> acc (cond-> pv' (conj pv'))) tgt)))))

(defn- reverse-pth-vecs
  [width pth-vecs]
  (->> pth-vecs
       normalize-path-vecs
       reverse-pth-vec-pnts))

(defn- map-pnts [f pth-vecs]
  (->> pth-vecs
       (map (fn [[op-k & pnts :as pth-vec]]
              (case op-k
                :A (-> pth-vec (update 3 f))
                (cons op-k
                      (map f pnts)))))))

(defn scale-path-vecs [pth-vecs ctr n]
  (map-pnts #(u/tl-point-towards % ctr n) pth-vecs))

(defn- add-ctrl-pnt-meta [args k i-pth-vec]
  (let [ctrl-cnt (dec (count args))]
    (map-indexed (fn [i-pnt pnt]
                   (if (< i-pnt ctrl-cnt)
                     (vary-meta pnt merge
                                {:i-tgt (if (and (= 2 ctrl-cnt)
                                                 (= 0 i-pnt))
                                          (dec i-pth-vec)
                                          i-pth-vec)})
                     pnt))
                 args)))

(defn pth-vec-->svg-seq [i-pth-vec pth-vec]
  (let [[k opts i-pnt0 args] (parse-vec pth-vec)
        args' (-> args (cond-> (pv/has-cp? k)
                               (add-ctrl-pnt-meta k i-pth-vec)))
        data [args' i-pth-vec i-pnt0 k]]
    (case k
      :arc  [data (arcs args opts)]
      :arc* [data (arcs args (assoc opts :ctd? true))]
      :M    [data (cons "M" args)]
      :L    [data (cons "L" args)]
      :l    [data (cons "l" args)]
      :A    (let [[r  p  tgt] args] [data (list "A" r  p   tgt)])
      :S    (let [[   c2 tgt] args] [data (list "S"    c2  tgt)])
      :s    (let [[   c2 tgt] args] [data (list "s"    c2  tgt)])
      :C    (let [[c1 c2 tgt] args] [data (list "C" c1 c2  tgt)])
      :c    (let [[c1 c2 tgt] args] [data (list "c" c1 c2  tgt)])
      :C1   (let [[c1    tgt] args] [data (list "C" c1 tgt tgt)])
      :c1   (let [[c1    tgt] args] [data (list "c" c1 tgt tgt)])
      :Q    (let [[c     tgt] args] [data (list "Q" c      tgt)])
      :q    (let [[c     tgt] args] [data (list "q" c      tgt)])
      :T    (let [[c1    tgt] args] [data (list "T" c1 tgt tgt)])
      :t    (let [[c1    tgt] args] [data (list "t" c1 tgt tgt)]))))

(defn path
  ([pth-vecs] (path nil pth-vecs))
  ([{:keys [mode debug? close? cutout? draw? width mirror]
     [scale-ctr scale-fract :as scale] :scale
     :or   {width 100
            mode  :concave}}
    pth-vecs]
   (let [pth-vecs' (-> pth-vecs
                       (cond-> scale
                               (scale-path-vecs scale-ctr scale-fract)))
         pnt-tups
         (for [[pth-vec-i pth-vec] (map-indexed vector pth-vecs')]
           (pth-vec-->svg-seq pth-vec-i pth-vec))

         points
         (-> pnt-tups
             (->> (map second))
             (cond-> mirror
                     (as-> pnts
                           (->> (case mirror
                                  nil       pth-vecs'
                                  :merged   (->> pth-vecs' (reverse-pth-vecs width))
                                  :separate (->> pth-vecs' normalize-path-vecs))
                                (mirror-pth-vecs width)
                                (path {:debug? true})
                                first
                                (map second)
                                (concat pnts))))
             (cond-> close? (concat ["Z"]))
             flatten)]

     (if debug?
       [pnt-tups points]
       points))))
