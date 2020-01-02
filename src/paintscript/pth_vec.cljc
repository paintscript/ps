(ns paintscript.pth-vec)

;; -----------------------------------------------------------------------------
;; classes

(def s-curves  #{:S :s})
(def has-cp?   #{:S :s
                 :C :c
                 :Q :q})
(def relative? #{:c :s})
(def short?    #{:S :C1 :T :arc})


;; -----------------------------------------------------------------------------
;; normalize (via tgt-prev & cp-prev)

(defn- dispatch-on-k [[k] & _] k)

(defn- flip-c2 [c2 tgt]
  (let [delta (mapv - tgt c2)]
    (mapv + tgt delta)))

;; rel->abs

(defmulti ^:private rel->abs dispatch-on-k)

(defmethod rel->abs :c [[k & pnts :as pv] tgt-prev _cp-prev]
  (let [[c1 c2 tgt :as pnts'] (map #(mapv + % tgt-prev) pnts)]
    [[:C c1 c2 tgt] tgt c2]))

(defmethod rel->abs :s [[k & pnts :as pv] tgt-prev cp-prev]
  (let [[c2 tgt] (map #(mapv + % tgt-prev) pnts)
        c1 (flip-c2 cp-prev tgt-prev)]
    [[:C c1 c2 tgt] tgt c2]))

;; short->full

(defmulti ^:private short->full dispatch-on-k)

(defmethod short->full :S [[k & pnts :as pv] tgt-prev cp-prev]
  (let [[c2 tgt] pnts
        c1 (flip-c2 cp-prev tgt-prev)]
    [[:C c1 c2 tgt] tgt c2]))

(defmethod short->full :C1 [[k & pnts :as pv] tgt-prev cp-prev]
  (let [[c1 tgt] pnts]
    [[:C c1 tgt tgt] tgt nil]))

(defmethod short->full :arc [[k & pnts :as pv] _ _]
  (let [[arg1 & args-rest] pnts
        arc' (if (map? arg1) pv (concat [:arc {}] pnts))]
    [arc' (last args-rest) nil]))

;; ------------------------------------
;; extraction

;; curve->target-cp

(defmulti ^:private curve->tgt-cp dispatch-on-k)
(defmethod curve->tgt-cp :C [[_ _ c2 tgt :as pv] _ _] [pv tgt c2])

;; pv->tgt

(defn pv->tgt  [pv] (last pv))
(defn pv->tgt' [pv _ _] [pv (last pv)])

;; ------------------------------------
;; dispatch

(defn normalization-steps [pv-k]
  (or (seq (concat (when (relative? pv-k) [rel->abs])
                   (when (short?    pv-k) [short->full])))
      (when (has-cp? pv-k) [curve->tgt-cp])
      [pv->tgt']))
