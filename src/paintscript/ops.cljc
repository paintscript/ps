(ns paintscript.ops
  (:require [paintscript.util :as u]
            [paintscript.els :as els]))

;; generic

(defn vec-insert
  [coll i el]
  (vec
   (concat (subvec coll 0 i)
           [el]
           (subvec coll i (count coll)))))

(defn vec-replace [coll i el]
  (vec
   (concat (subvec coll 0 i)
           [el]
           (subvec coll (inc i) (count coll)))))

(defn vec-remove
  [coll i]
  (vec
   (concat (subvec coll 0 i)
           (subvec coll (inc i) (count coll)))))

(defn vec-append [coll i el] (vec-insert coll (inc i) el))

;; info

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

(defn tail-iii [{:as params :keys [script]}]
  (let [pi  (-> script count (- 1))
        eli (-> script (get pi) count (- 1))
        xyi (-> script (get-in [pi eli]) count (- 1))]
    [:script pi eli xyi]))

;; points

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
         (update eli vec-append xyi' pnt)))))

(defn del-pnt
  [els eli xyi]
  (let [xyi' (+ xyi xyi-offset)]
    (-> els
        (update eli vec-remove xyi'))))

;; els

(defn infer-succ-el [els eli]
  (let [[k & pnts :as el-1] (get els eli)]
    (case k
      :M [:L (offset-pnt (last pnts) [10 10])]
      :L [:L (offset-pnt (last pnts) [10 10])]
      :C (let [tgt (offset-pnt (last pnts) [10 10])]
           [:C (last pnts) tgt tgt])
      :S (let [tgt (offset-pnt (last pnts) [10 10])]
           [:S tgt tgt]))))

(defn append-el
  ([els eli] (append-el els eli (infer-succ-el els eli)))
  ([els eli el]
   (if eli
     (-> els (vec-append eli el))
     [el])))

(defn del-el
  [els eli]
  (-> els
      (vec-remove eli)))

(defn transform-el
  [els eli to]
  (let [[el-k :as el] (get els eli)
        [el-1-k :as el-1] (get els (dec eli))
        el' (case el-k
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
    (-> els
        (vec-replace eli el'))))

;; params

(defn append-pth
  [script pi]
  (-> script
      (vec-append pi [:path {} [:M [10 10]]])))

(defn del-pth [script pi] (-> script (vec-remove pi)))

(defn tl-pth [params ii tl]
  (-> params
      (els/update-px (take 2 ii)
                     #(-> (els/translate-els % tl)
                          (cond-> (= :defs (first ii))
                                  vec)))))


(defn scale
  ([params ii c n] (-> params (els/update-px ii  els/scale-els c n)))
  ([params    c n] (-> params (els/update-px-all els/scale-els c n))))

(defn absolute
  ([params ii] (-> params (els/update-px ii  els/normalize-els :op :absolute)))
  ([params]    (-> params (els/update-px-all els/normalize-els :op :absolute))))

(defn full
  ([params ii] (-> params (els/update-px ii  els/normalize-els :op :full)))
  ([params]    (-> params (els/update-px-all els/normalize-els :op :full))))

(defn normalize
  ([params ii] (-> params (els/update-px ii  els/normalize-els :op :all)))
  ([params]    (-> params (els/update-px-all els/normalize-els :op :all))))

(defn update-p-opts [params ii f & args]
  (let [p-opts-i (concat (take 2 ii) [1])]
    (apply update-in params p-opts-i f args)))
