(ns paintscript.ops
  (:require [paintscript.util :as u]
            [paintscript.els :as els]
            [paintscript.nav :as nav]
            [paintscript.paint :as paint]
            [paintscript.conv :as conv]))

;; --- info

(defn has-pred? [pi] (pos? pi))

(defn el-len [el]
  (-> el count dec))

(defn get-tgt [el]
  (last el))

(defn offset-pnt [pnt delta] (mapv + pnt delta))
(defn xy-delta  [pnt1 pnt2] (mapv - pnt1 pnt2))

(defn get-pnt [els [eli xyi-abs :as ii]] (get-in els ii))

(defn get-pred-tgt [els eli]
  (-> (get els (-> eli (- 1))) get-tgt))

(defn tail-iii [{:as cmpt :keys [script]}]
  (let [pi  (-> script count (- 1) (max 0))
        eli (-> script (get pi) count (- 1) (max 1))
        xyi (some-> script (get-in [pi eli]) count (- 1))]
    [:script pi eli xyi]))

;; --- points

(def xyi-offset 1)
(def el-i-offset 2)

(defn infer-succ-pnt
  [els eli xyi]
  (let [el (get els eli)
        xyi'  (+ xyi xyi-offset)
        pnt-0   (get-pnt els [eli xyi'])]
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
   (let [xyi' (+ xyi xyi-offset)]
     (-> els
         (update eli u/vec-append xyi' pnt)))))

(defn del-pnt
  [els eli xyi]
  (let [xyi' (+ xyi xyi-offset)]
    (-> els
        (update eli u/vec-remove xyi'))))

;; --- els

(defn infer-succ-el [els eli]
  (let [[k & pnts :as el-1] (get els eli)]
    (case k
      :M [:L (offset-pnt (last pnts) [10 10])]
      :L [:L (offset-pnt (last pnts) [10 10])]
      :C (let [tgt (offset-pnt (last pnts) [10 10])]
           [:S (last pnts) tgt])
      :S (let [tgt (offset-pnt (last pnts) [10 10])]
           [:S tgt tgt]))))

(defn append-p-el
  ([p-els p-el-i] (append-p-el p-els p-el-i (infer-succ-el p-els p-el-i)))
  ([p-els p-el-i p-el]
   (cond
     p-el-i      (-> p-els (u/vec-append p-el-i p-el))
     (seq p-els) (append-p-el p-els (-> p-els count dec) p-el)

     ;; NOTE: only works for :defs entry (?)
     :else       [p-el])))

(defn del-el
  [els eli]
  (-> els
      (u/vec-remove eli)))

(defn p-el-prev [pth p-el-i]
  (let [v (get pth (dec p-el-i))]
    (when (vector? v)
      v)))

(defn transform-el
  [pth p-el-i to]
  (let [[p-el-k :as el] (get pth p-el-i)
        [el-1-k :as el-1] (p-el-prev pth p-el-i)
        p-el' (case p-el-k
                :L (let [tgt (last el)]
                     (case to
                       :S [:S tgt tgt]
                       :Q [:Q tgt tgt]
                       :C [:C (last el-1) tgt tgt]))
                :C (let [[_ c1 c2 tgt] el]
                     (case to
                       :S [:S c2 tgt]
                       :Q [:Q c2 tgt]
                       :L [:L tgt]))
                :S (let [[_ c tgt] el]
                     (case to
                       :L [:L tgt]
                       :Q [:Q c tgt]
                       :C [:C (last el-1) c tgt]))
                :Q (let [[_ c tgt] el]
                     (case to
                       :L [:L tgt]
                       :S [:S c tgt]
                       :C [:C (last el-1) c tgt])))]
    (-> pth
        (u/vec-replace p-el-i p-el'))))

;; --- cmpt

(defn append-pth
  [script pi]
  (-> script
      (u/vec-append pi [:path {} [:M [10 10]]])))

(defn del-pth [script pi] (-> script (u/vec-remove pi)))

(defn translate
  ([cmpt sel n] (-> cmpt (els/update-s-el-sel sel els/translate-p-els n)))
  ([cmpt     n] (-> cmpt (els/update-s-els        els/translate-p-els n))))

(defn rotate
  ([cmpt sel c a] (-> cmpt (els/update-s-el-sel sel els/rotate-p-els c a)))
  ([cmpt     c a] (-> cmpt (els/update-s-els        els/rotate-p-els c a))))

(defn scale
  ([cmpt sel c n] (-> cmpt (els/update-s-el-sel sel els/scale-p-els c n)))
  ([cmpt     c n] (-> cmpt (els/update-s-els        els/scale-p-els c n))))

(defn absolute
  ([cmpt sel] (-> cmpt (els/update-s-el-sel sel els/normalize-p-els :op :rel->abs)))
  ([cmpt]     (-> cmpt (els/update-s-els        els/normalize-p-els :op :rel->abs))))

(defn full
  ([cmpt sel] (-> cmpt (els/update-s-el-sel sel els/normalize-p-els :op :short->full)))
  ([cmpt]     (-> cmpt (els/update-s-els        els/normalize-p-els :op :short->full))))

(defn normalize
  ([cmpt sel] (-> cmpt (els/update-s-el-sel sel els/normalize-p-els :op :all)))
  ([cmpt]     (-> cmpt (els/update-s-els        els/normalize-p-els :op :all))))

(defn mirror
  ([cmpt axis pos sel] (-> cmpt (els/update-s-el-sel sel #(els/mirror-p-els axis pos %))))
  ([cmpt axis pos]     (-> cmpt (els/update-s-els        #(els/mirror-p-els axis pos %)))))

(defn reverse-path
  ([cmpt sel] (-> cmpt (els/update-s-el-sel sel  els/reverse-p-els)))
  ([cmpt]     (-> cmpt (els/update-s-els els/reverse-p-els))))

(defn update-p-opts [cmpt sel-rec f & args]
  (let [pth-vec (-> sel-rec
                    (assoc :p-el-i 1)
                    nav/pth-rec->vec)]
    (apply update-in cmpt pth-vec f args)))

(defn toggle-d [cmpt sel-rec]
  (let [pth-vec (-> sel-rec
                    nav/pth-rec->vec)]
    (update-in cmpt pth-vec
               (fn [[_pth {:as p-opts :keys [d]} & p-els]]
                 (cond
                   d     (vec
                          (concat [:path (-> p-opts (dissoc :d))]
                                  (conv/path-d->els d)))
                   :else [:path (-> p-opts
                                    (assoc :d (paint/path-str cmpt p-opts p-els)))])))))
