(ns paintscript.els
  (:require [paintscript.util :as u]
            [paintscript.el :as el]
            [paintscript.nav :as nav]))

(def eli0 2)
(def i-pnt0 1)

(defn- arcs
  [arc-xys {:as opts :keys [mode ctd?] :or {mode :concave}}]
  (let [[head & tail]    arc-xys
        paired-with-prev (map vector tail (drop-last arc-xys))
        arc-middle       (str (case mode :concave "0,1" :convex "0,0") )]
    (concat (when-not ctd?
              [["M" head]])
            (for [[[x  y  :as _xy]
                   [xp yp :as _xy-prev]] paired-with-prev]
              ["A" [(- x xp) (- y yp)] [0 arc-middle] [x y]]))))

(defn- parse-el [[el-k & args]]
  (if (map? (first args))
    [el-k (first args) 2 (rest args)]
    [el-k nil 1 args]))

(defn- mirror-xy  [width pnt]  (update pnt 0 #(- width %)))
(defn- mirror-xys [width xys] (map #(mirror-xy width %) xys))

(defn- mirror-els [width els]
  (->> els
       (map (fn [[el-k & xys :as el]]
              (case el-k
                :arc (let [[opts & xys] xys]
                       (concat [el-k (-> opts
                                          (update :mode
                                                  #(case % :convex :concave :convex)))]
                               (mirror-xys width xys)))
                :A (let [[r [rot f1 f2] xy] xys

                               ;; NOTE: needs to be undone when combined with reverse
                               f2' (-> f2 el/flip-bin)]
                           [:A r [rot f1 f2'] (mirror-xy width xy)])
                (cons el-k
                      (mirror-xys width xys)))))))

(defn normalize-els [els]
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
                    (el/normalization-steps el-k))]
        (recur els-tail tgt' cp' (conj acc el'))))))

(defn- with-abs-meta [xy1 xy2] (with-meta xy1 {:abs xy2}))
(defn abs-meta [xy] (-> xy meta :abs))

(defn attach-normalized-meta [els els']
  (map (fn [[el-k & xys :as el] [el' & xys']]
         (case el-k
           :c (vec (cons el-k (map with-abs-meta xys xys')))
           :s (let [[  c2  tgt]  xys
                    [_ c2' tgt'] xys']
                [el-k (with-abs-meta c2 c2') (with-abs-meta tgt tgt')])
           el))
       els
       els'))

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

(defn- map-xys [f els]
  (->> els
       (map (fn [[el-k & xys :as el]]
              (case el-k
                :A (-> el (update 3 f))
                (cons el-k
                      (map f xys)))))))

(defn scale-els [els ctr n]
  (map-xys #(u/tl-point-towards % ctr n) els))

(defn- add-cp-meta [args eli]
  (let [ctrl-cnt (dec (count args))]
    (map-indexed (fn [i-pnt pnt]
                   (if (< i-pnt ctrl-cnt)
                     (vary-meta pnt merge
                                {:i-tgt (if (and (= 2 ctrl-cnt)
                                                 (= 0 i-pnt))
                                          (dec eli)
                                          eli)})
                     pnt))
                 args)))

(defn- el-->svg-seq [eli el]
  (let [[el-k opts i-pnt0 args] (parse-el el)
        args' (-> args (cond-> (el/has-cp? el-k)
                               (add-cp-meta eli)))
        data [args' eli i-pnt0 el-k]]
    (case el-k
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
      :t    (let [[c1    tgt] args] [data (list "t" c1 tgt tgt)])
      :z    [data (list "z")])))

(defn mirror-els* [mode width els]
  (sequence
   (comp
    (partition-by #{[:z]})
    (mapcat
     (fn [els-part]
       (if (= (list [:z]) els-part)
         els-part
         (concat
          els-part
          (->> (case mode
                 :merged   (->> els-part (reverse-els width))
                 :separate (->> els-part normalize-els))
               (mirror-els width)))))))
   els))

(defn path
  ([els] (path nil els))
  ([{:keys [mode debug? close? cutout? draw? width mirror coords? coord-size]
     [scale-ctr scale-fract :as scale] :scale
     :or   {width 100
            mode  :concave}}
    els]
   (let [els'
         (-> els
             (cond-> scale  (scale-els scale-ctr scale-fract)
                     (and mirror
                          (not coords?)) (->> (mirror-els* mirror width))))

         pnt-tups
         (for [[eli el] (map-indexed vector els')]
           (el-->svg-seq eli el))

         points
         (-> pnt-tups
             (->> (map second))
             (cond-> close? (concat ["Z"]))
             flatten)]

     (if debug?
       [pnt-tups points]
       points))))
