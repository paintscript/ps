(ns paintscript.el
  (:require [clojure.set :as set]
            [paintscript.util :as u]))

(defn flip-bin [n] (case n 0 1 0))

;; -----------------------------------------------------------------------------
;; classes

(def has-cp?   #{:S :s
                 :C :c :C1 :c1
                 :Q :q})
(def relative? #{:c :s :l :m :q :t :v :h})
(def absolute? #{:C :S :L :M :z})
(def short?    #{:S :C1 :T :arc :V :H})

(def el?       (set/union has-cp? relative? absolute? short?))

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
  (let [[c2 tgt] (map #(u/v+ % tgt-prev) pnts)
        c1 (flip-c2 cp-prev tgt-prev)]
    [[:C c1 c2 tgt] tgt c2]))

(defmethod rel->abs :q [[_k & pnts :as elv] tgt-prev _cp-prev]
  (let [[c1 tgt :as pnts'] (map #(u/v+ % tgt-prev) pnts)]
    [[:Q c1 tgt] tgt c1]))

(defmethod rel->abs :t [[_k & pnts :as elv] tgt-prev cp-prev]
  (let [[tgt] (map #(u/v+ % tgt-prev) pnts)
        c1 (flip-c2 cp-prev tgt-prev)]
    [[:T tgt] tgt c1]))

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
    [[:T c1 tgt] tgt c1]))

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

;; --- curve->target-cp

(defmulti ^:private curve->tgt-cp dispatch-on-k)
(defmethod curve->tgt-cp :C [[_ _ c2 tgt :as el] _ _] [el tgt c2])
(defmethod curve->tgt-cp :Q [[_   c  tgt :as el] _ _] [el tgt c])
(defmethod curve->tgt-cp :S [[_   c  tgt :as el] _ _] [el tgt c])

;; --- el->tgt

(defn el->tgt  [el] (-> el rest last)) ;; NOTE: rest for singleton-vecs like [:z]
(defn el->tgt' [el tgt-prev _]
  (case (first el)
    (:Z :z) [el tgt-prev]
    [el (el->tgt el)]))

;; ------------------------------------
;; dispatch

(defn normalization-steps [el-k op]
  (or (seq (concat (when (and (#{:all :absolute} op) (relative? el-k)) [rel->abs])
                   (when (and (#{:all :full} op)     (short?    el-k)) [short->full])))
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
