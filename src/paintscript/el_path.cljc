(ns paintscript.el-path
  "https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorial/Paths"
  (:require [clojure.set :as set]
            [paintscript.util :as u]))

(defn flip-bin [n] (case n 0 1 0))

;; -----------------------------------------------------------------------------
;; path extensions

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

(defn- circle-path [{:as opts :keys [r], [cx cy] :center}]
  (list "M" [cx cy] "m" [(- r) 0]
        "a" [r r] [0 1 0] [(* 2 r) 0]
        "a" [r r] [0 1 0] [(- (* 2 r)) 0]))

;; -----------------------------------------------------------------------------
;; classes

(def has-cp?   #{      :c :q,          :s     :c1
                       :C :Q,          :S     :C1})
(def relative? #{:m :l :c :q,    :v :h :s :t, :c1, :a})
(def absolute? #{:M :L :C :Q :z, :V :H :S :T, :C1, :A :arc})
(def full?     #{:m :l :c :q
                 :M :L :C :Q :z})
(def short?    #{                :v :h :s :t, :c1
                                 :V :H :S :T, :C1,    :arc})

(def el?       (set/union relative? absolute?))

(defn ref? [x]
  (and (vector? x)
       (= :ref (get x 0))))


;; -----------------------------------------------------------------------------
;; normalize (via tgt-prev & cp-prev)

(defn- dispatch-on-k [[k] & _] k)

(defn- flip-c2 [c2 tgt]
  (let [delta (mapv - tgt c2)]
    (u/v+ tgt delta)))

;; --- rel->abs

(defmulti ^:private rel->abs dispatch-on-k)

(defmethod rel->abs :c [[_k & pnts :as elv] tgt-prev _cp-prev]
  (let [[c1 c2 tgt :as pnts'] (map #(u/v+ % tgt-prev) pnts)]
    [[:C c1 c2 tgt] tgt c2]))

(defmethod rel->abs :s [[_k & pnts :as elv] tgt-prev cp-prev]
  (let [[c2 tgt] (map #(u/v+ % tgt-prev) pnts)]
    [[:S c2 tgt] tgt c2]))

(defmethod rel->abs :q [[_k & pnts :as elv] tgt-prev _cp-prev]
  (let [[c1 tgt :as pnts'] (map #(u/v+ % tgt-prev) pnts)]
    [[:Q c1 tgt] tgt c1]))

(defmethod rel->abs :t [[_k & pnts :as elv] tgt-prev cp-prev]
  (let [[tgt] (map #(u/v+ % tgt-prev) pnts)
        c1 (flip-c2 cp-prev tgt-prev)]
    [[:T tgt] tgt c1]))

(defmethod rel->abs :a [[_k r p tgt :as elv] tgt-prev _cp-prev]
  (let [tgt' (u/v+ tgt tgt-prev)]
    [[:A r p tgt'] tgt' nil]))

(defn- rel->abs--pnt-seq [tgt-prev pnt-seq]
  (first
   (reduce (fn [[vv tgt-prev] v-rel]
             (let [v-abs (u/v+ v-rel tgt-prev)]
               [(conj vv v-abs) v-abs]))
           [[] tgt-prev]
           pnt-seq)))

(defmethod rel->abs :l [[_k & pnts :as elv] tgt-prev _]
  (let [pnts' (rel->abs--pnt-seq tgt-prev pnts)]
    [(vec (cons :L pnts')) (last pnts') nil]))

(defmethod rel->abs :v [[_k & pnts :as elv] tgt-prev _]
  (let [pnts' (map second
                   (rel->abs--pnt-seq tgt-prev (map (fn [y] [0 y]) pnts)))]
    [(vec (cons :V pnts')) [0 (last pnts')] nil]))

(defmethod rel->abs :h [[_k & pnts :as elv] tgt-prev _]
  (let [pnts' (map first
                   (rel->abs--pnt-seq tgt-prev (map (fn [x] [x 0]) pnts)))]
    [(vec (cons :H pnts')) [(last pnts') 0] nil]))

(defmethod rel->abs :m [[_k & pnts :as elv] tgt-prev _]
  (let [pnts' (rel->abs--pnt-seq tgt-prev pnts)]
    [(vec (cons :M pnts')) (last pnts') nil]))

;; --- short->full

(defmulti ^:private short->full dispatch-on-k)

(defmethod short->full :S [[_k & pnts :as elv] tgt-prev cp-prev]
  (let [[c2 tgt] pnts
        c1 (flip-c2 cp-prev tgt-prev)]
    [[:C c1 c2 tgt] tgt c2]))

(defmethod short->full :C1 [[_k & pnts :as elv] tgt-prev cp-prev]
  (let [[c1 tgt] pnts]
    [[:C c1 tgt tgt] tgt nil]))

(defmethod short->full :T [[_k & pnts :as elv] tgt-prev cp-prev]
  (let [[tgt] pnts
        c1 (flip-c2 cp-prev tgt-prev)]
    [[:Q c1 tgt] tgt c1]))

(defmethod short->full :V [[_k & pnts :as elv] tgt-prev cp-prev]
  (let [[y] pnts
        tgt [(first tgt-prev) y]]
    [[:L tgt] tgt]))

(defmethod short->full :H [[_k & pnts :as elv] tgt-prev cp-prev]
  (let [[x] pnts
        tgt [x (second tgt-prev)]]
    [[:L tgt] tgt]))

(defmethod short->full :arc [[_k & pnts :as elv] _ _]
  (let [[arg1 & args-rest] pnts
        arc' (if (map? arg1) elv (concat [:arc {}] pnts))]
    [arc' (last args-rest) nil]))

;; ------------------------------------
;; extraction

;; --- el->cp-i

(defn el->cp-i [el pos-k]
  (case (first el)
    :C (case pos-k :init 1   :term 2)
    :c (case pos-k :init 1   :term 2)
    :Q (case pos-k :init nil :term 1)
    :q (case pos-k :init nil :term 1)
    :S (case pos-k :init nil :term 1)
    :s (case pos-k :init nil :term 1)
    nil))

;; --- curve->tgt-cp

(defmulti ^:private curve->tgt-cp dispatch-on-k)
(defmethod curve->tgt-cp :C [[_ _ c2 tgt :as el] _ _] [el tgt c2])
(defmethod curve->tgt-cp :Q [[_   c  tgt :as el] _ _] [el tgt c])
(defmethod curve->tgt-cp :S [[_   c  tgt :as el] _ _] [el tgt c])

;; --- el->tgt

(defn el->tgt  [el] (-> el rest last)) ;; NOTE: rest is for singleton-vecs like [:z]
(defn el->tgt' [el tgt-prev _]
  (case (first el)
    (:Z :z) [el tgt-prev]
    [el (el->tgt el)]))

;; ------------------------------------
;; dispatch

(defn normalization-steps
  ;; NOTE: short->full is only implemented on abs els, necessitating :rel->abs
  [el-k op]
  (or (some-> (concat
               (when (and (#{:all :rel->abs :short->full} op) (relative? el-k)) [rel->abs])
               (when (and (#{:all :short->full} op) (short? el-k)) [short->full]))
              seq)
      (when (has-cp? el-k) [curve->tgt-cp])
      [el->tgt']))


;; -----------------------------------------------------------------------------
;; reverse

(defmulti el->reversed dispatch-on-k)

(defmethod el->reversed :M [[_ & pnts :as el] tgt-prev]
  [(when tgt-prev [:M tgt-prev])
   (last pnts)])

(defmethod el->reversed :L [[_ & pnts] tgt-prev]
  [(vec (->> pnts butlast (cons tgt-prev) reverse (cons :L)))
   (last pnts)])

(defmethod el->reversed :arc [[_ & pnts] tgt-prev]
  [(vec (-> pnts butlast (cons tgt-prev) reverse (cons :arc*)))
   (last pnts)])

(defmethod el->reversed :A [[_ r p tgt] tgt-prev]
  (let [p' (update p 2 flip-bin)]
    [[:A r p' tgt-prev] tgt]))

(defmethod el->reversed :C [[_ c1 c2 tgt] tgt-prev]
  [[:C c2 c1 tgt-prev] tgt])

(defmethod el->reversed :Q [[_ c tgt] tgt-prev]
  [[:Q c tgt-prev] tgt])

(defmethod el->reversed :z [_ tgt-prev]
  [[:z] tgt-prev])


;; -----------------------------------------------------------------------------
;; serialize / 'paint'

(defn- el->out
  [{:as el :keys [el-k opts args]}]
  (case el-k
    :M    (cons "M" args)
    :m    (cons "m" args)
    :L    (cons "L" args)
    :l    (cons "l" args)

    :V    (cons "V" args)
    :H    (cons "H" args)
    :v    (cons "v" args)
    :h    (cons "h" args)

    :z    (list "z")
    :A    (let [[r  p  tgt] args] (list "A" r  p   tgt))
    :a    (let [[r  p  tgt] args] (list "a" r  p   tgt))
    :Q    (let [[c     tgt] args] (list "Q" c      tgt))
    :q    (let [[c     tgt] args] (list "q" c      tgt))
    :T    (let [[c1    tgt] args] (list "T" c1 tgt tgt))
    :t    (let [[c1    tgt] args] (list "t" c1 tgt tgt))
    :S    (let [[   c2 tgt] args] (list "S"    c2  tgt))
    :s    (let [[   c2 tgt] args] (list "s"    c2  tgt))
    :C    (let [[c1 c2 tgt] args] (list "C" c1 c2  tgt))
    :c    (let [[c1 c2 tgt] args] (list "c" c1 c2  tgt))

    ;; --- extensions
    :C1   (let [[c1    tgt] args] (list "C" c1 tgt tgt))
    :c1   (let [[c1    tgt] args] (list "c" c1 tgt tgt))

    :circle (circle-path opts)
    :arc    (arcs args opts)
    :arc*   (arcs args (assoc opts :ctd? true))))

(defn els->out [els] (->> els (map el->out) flatten))