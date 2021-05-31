(ns paintscript.data-ops-path
  "https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths"
  (:require [clojure.set :as set]
            [paintscript.util :as u]))

(defn flip-bin [n] (case n 0 1 0))

(defn merge-el-tup [[el-k'
                     el-argv'] el-rec]
  (-> el-rec
      (assoc :el-k    el-k'
             :el-argv el-argv')))

;; -----------------------------------------------------------------------------
;; path extensions

(defn- arcs
  [arc-xys {:as opts :keys [mode ctd?] :or {mode :concave}}]
  (let [[head & tail]    arc-xys
        paired-with-prev (map vector tail (drop-last arc-xys))
        arc-middle       (str (case mode :concave "0,1" :convex "0,0"))]
    (concat (when-not ctd?
              [["M" head]])
            (for [[[x  y  :as _xy]
                   [xp yp :as _xy-prev]] paired-with-prev]
              ["A" [(- x xp) (- y yp)] [0 arc-middle] [x y]]))))

(defn- circle-path [{:as opts :keys [r], [cx cy] :center}]
  (list "M" [cx cy] "m" [(- r) 0]
        "a" [r r] [0 1 0] [(* 2 r) 0]
        "a" [r r] [0 1 0] [(- (* 2 r)) 0]))

;; -----------------------------------------------------------------------------
;; classes

(def has-cp?   #{      :c :q,          :s     :c1
                       :C :Q,          :S     :C1})
(def relative? #{:m :l :c :q :z, :v :h :s :t, :c1, :a})
(def absolute? #{:M :L :C :Q :Z, :V :H :S :T, :C1, :A :arc})
(def full?     #{:m :l :c :q :z
                 :M :L :C :Q :Z})
(def short?    #{                :v :h :s :t, :c1
                                 :V :H :S :T, :C1,    :arc})

(def el?       (set/union relative? absolute?))

(defn ref? [x] (-> x :el-k (= :ref)))


;; -----------------------------------------------------------------------------
;; normalize (via tgt-prev & cp-prev)

(defn- dispatch-on-k [elr & _] (:el-k elr))

(defn- flip-c2 [c2 tgt]
  (let [delta (mapv - tgt c2)]
    (u/v+ tgt delta)))

;; --- rel->abs

(defmulti ^:private rel->abs dispatch-on-k)

(defmethod rel->abs :z [_ _ _] [[:Z]])

(defmethod rel->abs :c [{:as el, xys :el-argv} tgt-prev _cp-prev]
  (let [[c1 c2 tgt] (map #(u/v+ % tgt-prev) xys)]
    [[:C [c1 c2 tgt]] tgt c2]))

(defmethod rel->abs :s [{:as el, xys :el-argv} tgt-prev cp-prev]
  (let [[c2 tgt] (map #(u/v+ % tgt-prev) xys)]
    [[:S [c2 tgt]] tgt c2]))

(defmethod rel->abs :q [{:as el, xys :el-argv} tgt-prev _cp-prev]
  (let [[c1 tgt] (map #(u/v+ % tgt-prev) xys)]
    [[:Q [c1 tgt]] tgt c1]))

(defmethod rel->abs :t [{:as el, xys :el-argv} tgt-prev cp-prev]
  (let [[tgt] (map #(u/v+ % tgt-prev) xys)
        c1 (flip-c2 cp-prev tgt-prev)]
    [[:T [tgt]] tgt c1]))

(defmethod rel->abs :a [{:as el, [rxy params tgt] :el-argv} tgt-prev _cp-prev]
  (let [tgt' (u/v+ tgt tgt-prev)]
    [[:A [rxy params tgt']] tgt' nil]))

(defn- rel->abs--pnt-seq [tgt-prev pnt-seq]
  (first
   (reduce (fn [[vv tgt-prev] v-rel]
             (let [v-abs (u/v+ v-rel tgt-prev)]
               [(conj vv v-abs) v-abs]))
           [[] tgt-prev]
           pnt-seq)))

(defmethod rel->abs :l [{:as el, xys :el-argv} tgt-prev _]
  (let [xys' (rel->abs--pnt-seq tgt-prev xys)]
    [[:L xys'] (last xys') nil]))

(defmethod rel->abs :v [{:as el, xys :el-argv} tgt-prev _]
  (let [xys' (map second
                   (rel->abs--pnt-seq tgt-prev (map (fn [y] [0 y]) xys)))]
    [[:V xys'] [0 (last xys')] nil]))

(defmethod rel->abs :h [{:as el, xys :el-argv} tgt-prev _]
  (let [xys' (map first
                   (rel->abs--pnt-seq tgt-prev (map (fn [x] [x 0]) xys)))]
    [[:H xys'] [(last xys') 0] nil]))

(defmethod rel->abs :m [{:as el, xys :el-argv} tgt-prev _]
  (let [xys' (if tgt-prev
                (rel->abs--pnt-seq tgt-prev xys)
                xys)]
    [[:M xys'] (last xys') nil]))

;; --- short->full

(defmulti ^:private short->full dispatch-on-k)

(defmethod short->full :S [{:as el, [c2 tgt] :el-argv} tgt-prev cp-prev]
  (let [c1 (flip-c2 cp-prev tgt-prev)]
    [[:C [c1 c2 tgt]] tgt c2]))

(defmethod short->full :C1 [{:as el, [c1 tgt] :el-argv} tgt-prev cp-prev]
  [[:C [c1 tgt tgt]] tgt nil])

(defmethod short->full :T [{:as el, [tgt] :el-argv} tgt-prev cp-prev]
  (let [c1 (flip-c2 cp-prev tgt-prev)]
    [[:Q [c1 tgt]] tgt c1]))

(defmethod short->full :V [{:as el, [y] :el-argv} tgt-prev cp-prev]
  (let [tgt [(first tgt-prev) y]]
    [[:L [tgt]] tgt]))

(defmethod short->full :H [{:as el, [x] :el-argv} tgt-prev cp-prev]
  (let [tgt [x (second tgt-prev)]]
    [[:L [tgt]] tgt]))

(defmethod short->full :arc [{:as el, args :el-argv} _ _]
  [[:arc args] (last args) nil])

;; ------------------------------------
;; extraction

;; --- el->cp-i

(defn el->cp-i [el pos-k]
  (case (:el-k el)
    :C (case pos-k :init 0   :term 1)
    :c (case pos-k :init 0   :term 1)
    :Q (case pos-k :init nil :term 0)
    :q (case pos-k :init nil :term 0)
    :S (case pos-k :init nil :term 0)
    :s (case pos-k :init nil :term 0)
    nil))

;; --- curve->tgt-cp

(defmulti ^:private curve->tgt-cp dispatch-on-k)
(defmethod curve->tgt-cp :C [{:as el :keys [el-k], [_ c2 tgt :as xys] :el-argv} _ _] [[el-k xys] tgt c2])
(defmethod curve->tgt-cp :Q [{:as el :keys [el-k], [  c  tgt :as xys] :el-argv} _ _] [[el-k xys] tgt c])
(defmethod curve->tgt-cp :S [{:as el :keys [el-k], [  c  tgt :as xys] :el-argv} _ _] [[el-k xys] tgt c])

;; --- el->tgt

(defn el->tgt  [el] (-> el :el-argv peek))
(defn el->tgt' [{:as el :keys [el-k el-argv]} tgt-prev _]
  (case (:el-k el)
    (:Z :z) [[el-k el-argv] tgt-prev]
    ; [el (el->tgt el)]
    [[el-k el-argv] (el->tgt el)]))

;; ------------------------------------
;; dispatch

(defn normalization-steps
  ;; NOTE: short->full is only implemented on abs els, necessitating :rel->abs
  [p-el-k op]
  (or (some-> (concat
               (when (and (#{:all :rel->abs :short->full} op) (relative? p-el-k)) [[:rel->abs rel->abs]])
               (when (and (#{:all :short->full} op) (short? p-el-k)) [[:short->full short->full]]))
              seq)
      (when (has-cp? p-el-k) [[:curve->tgt-cp curve->tgt-cp]])
      [[:el->tgt el->tgt']]))


;; -----------------------------------------------------------------------------
;; reverse

(defn el->reversed [{:as el :keys [el-k], xys :el-argv} tgt-prev]
  (let [[xys'
         tgt-prev] (case el-k
                     :M   [(when tgt-prev [tgt-prev]) (peek xys)]
                     :L   [(->> xys butlast (cons tgt-prev) reverse vec) (peek xys)]
                     :arc [(->> xys butlast (cons tgt-prev) reverse vec) (peek xys)]
                     :A   (let [[r p tgt] xys
                                p' (update p 2 flip-bin)]
                            [[r p' tgt-prev] tgt])
                     :C   (let [[c1 c2 tgt] xys]
                            [[c2 c1 tgt-prev] tgt])
                     :Q   (let [[c tgt] xys]
                            [[c tgt-prev] tgt])
                     :z   [nil tgt-prev]
                     :Z   [nil tgt-prev])
        el-k' (when (= :arc el-k)
                :arc*)
        el'   (when-not (and (= :M el-k)
                             (nil? xys'))
                (-> el
                    (cond-> el-k' (assoc :el-k el-k'))
                    (assoc :el-argv xys')))]
    [el' tgt-prev]))


;; -----------------------------------------------------------------------------
;; serialize / 'paint'

(defn- p-el->out
  [{:as el :keys [el-k el-opts el-argv]}]
  (case el-k
    :M    (cons "M" el-argv)
    :m    (cons "m" el-argv)
    :L    (cons "L" el-argv)
    :l    (cons "l" el-argv)

    :V    (cons "V" el-argv)
    :H    (cons "H" el-argv)
    :v    (cons "v" el-argv)
    :h    (cons "h" el-argv)

    :z    (list "z")
    :Z    (list "Z")
    :A    (let [[r  p  tgt] el-argv] (list "A" r  p   tgt))
    :a    (let [[r  p  tgt] el-argv] (list "a" r  p   tgt))
    :Q    (let [[c     tgt] el-argv] (list "Q" c      tgt))
    :q    (let [[c     tgt] el-argv] (list "q" c      tgt))
    :T    (let [[c1    tgt] el-argv] (list "T" c1 tgt tgt))
    :t    (let [[c1    tgt] el-argv] (list "t" c1 tgt tgt))
    :S    (let [[   c2 tgt] el-argv] (list "S"    c2  tgt))
    :s    (let [[   c2 tgt] el-argv] (list "s"    c2  tgt))
    :C    (let [[c1 c2 tgt] el-argv] (list "C" c1 c2  tgt))
    :c    (let [[c1 c2 tgt] el-argv] (list "c" c1 c2  tgt))

    ;; --- extensions
    :C1   (let [[c1    tgt] el-argv] (list "C" c1 tgt tgt))
    :c1   (let [[c1    tgt] el-argv] (list "c" c1 tgt tgt))

    :circle (circle-path el-opts)
    :arc    (arcs el-argv el-opts)
    :arc*   (arcs el-argv (assoc el-opts :ctd? true))))

(defn p-els->out [els] (->> els (map p-el->out) flatten))
