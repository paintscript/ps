(ns paintscript.el
  (:require [clojure.set :as set]
            [paintscript.util :as u]))

(defn flip-bin [n] (case n 0 1 0))

;; -----------------------------------------------------------------------------
;; classes

(def has-cp?   #{:S :s
                 :C :c :C1 :c1
                 :Q :q})
(def relative? #{:c :s :l})
(def absolute? #{:C :S :L :M :z})
(def short?    #{:S :C1 :T :arc})

(def el?       (set/union has-cp? relative? absolute? short?))


;; -----------------------------------------------------------------------------
;; normalize (via tgt-prev & cp-prev)

(defn- dispatch-on-k [[k] & _] k)

(defn- flip-c2 [c2 tgt]
  (let [delta (mapv - tgt c2)]
    (u/v+ tgt delta)))

;; rel->abs

(defmulti ^:private rel->abs dispatch-on-k)

(defmethod rel->abs :c [[k & pnts :as el] tgt-prev _cp-prev]
  (let [[c1 c2 tgt :as pnts'] (map #(u/v+ % tgt-prev) pnts)]
    [[:C c1 c2 tgt] tgt c2]))

(defmethod rel->abs :s [[k & pnts :as el] tgt-prev cp-prev]
  (let [[c2 tgt] (map #(u/v+ % tgt-prev) pnts)
        c1 (flip-c2 cp-prev tgt-prev)]
    [[:C c1 c2 tgt] tgt c2]))

(defmethod rel->abs :l [[k & pnts :as el] tgt-prev _]
  (let [pnts' (map #(u/v+ % tgt-prev) pnts)]
    [(vec (cons :L pnts')) (last pnts') nil]))

;; short->full

(defmulti ^:private short->full dispatch-on-k)

(defmethod short->full :S [[k & pnts :as el] tgt-prev cp-prev]
  (let [[c2 tgt] pnts
        c1 (flip-c2 cp-prev tgt-prev)]
    [[:C c1 c2 tgt] tgt c2]))

(defmethod short->full :C1 [[k & pnts :as el] tgt-prev cp-prev]
  (let [[c1 tgt] pnts]
    [[:C c1 tgt tgt] tgt nil]))

(defmethod short->full :arc [[k & pnts :as el] _ _]
  (let [[arg1 & args-rest] pnts
        arc' (if (map? arg1) el (concat [:arc {}] pnts))]
    [arc' (last args-rest) nil]))

;; ------------------------------------
;; extraction

;; curve->target-cp

(defmulti ^:private curve->tgt-cp dispatch-on-k)
(defmethod curve->tgt-cp :C [[_ _ c2 tgt :as el] _ _] [el tgt c2])
(defmethod curve->tgt-cp :Q [[_   c  tgt :as el] _ _] [el tgt c])
(defmethod curve->tgt-cp :S [[_   c  tgt :as el] _ _] [el tgt c])

;; el->tgt

(defn el->tgt  [el] (-> el rest last)) ;; NOTE: rest for singleton-vecs like [:z]
(defn el->tgt' [el _ _] [el (el->tgt el)])

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
