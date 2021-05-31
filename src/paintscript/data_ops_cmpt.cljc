(ns paintscript.data-ops-cmpt
  (:require [paintscript.util :as u]
            [paintscript.nav :as nav]
            [paintscript.paint :as paint]
            [paintscript.conv :as conv]
            [paintscript.data :as data]
            [paintscript.data-ops :as data-ops]))

;; --- info

(defn has-pred? [pi] (pos? pi))

(defn el-len  [el] (-> el :el-argv count))
(defn get-tgt [el] (-> el :el-argv peek))

(defn offset-pnt [pnt delta] (mapv + pnt delta))
(defn xy-delta  [pnt1 pnt2] (mapv - pnt1 pnt2))

(defn get-pnt [els [eli xyi-abs]]
  (get-in els [eli :el-argv (or xyi-abs 0)]))

(defn get-pred-tgt [els eli]
  (-> (get els (-> eli (- 1))) get-tgt))

(defn tail-iii [{:as cmpt :keys [script]}]
  (let [pi  (-> script count (- 1) (max 0))
        eli (-> script (get pi) count (- 1) (max 1))
        xyi (some-> script (get-in [pi eli]) count (- 1))]
    [:script pi eli xyi]))

;; --- points

; (def xyi-offset 1)
; (def el-i-offset 2)

(defn infer-succ-pnt
  [els eli xyi]
  (let [el    (get els eli)
        xyi'  (+ xyi
                 ; xyi-offset
                 )
        pnt-0 (get-pnt els [eli xyi'])]
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
   (let [xyi' (or xyi 0
                 ; xyi-offset
                 )]
     (-> els
         (update-in [eli :el-argv] u/vec-append xyi' pnt)))))

(defn del-pnt
  [els eli xyi]
  (let [xyi' (+ xyi
                ; xyi-offset
                )]
    (-> els
        (update-in [eli :el-argv] u/vec-remove xyi'))))

;; --- els

(defn infer-succ-el [els eli]
  (let [{:as el-1 :keys [el-k], pnts :el-argv} (get els eli)]
    (case el-k
      :M (data/elem :el-k :L, :el-argv [(offset-pnt (last pnts) [10 10])])
      :L (data/elem :el-k :L, :el-argv [(offset-pnt (last pnts) [10 10])])
      :C (let [tgt (offset-pnt (last pnts) [10 10])]
           (data/elem :el-k :S, :el-argv [(last pnts) tgt]))
      :S (let [tgt (offset-pnt (last pnts) [10 10])]
           (data/elem :el-k :S, :el-argv [tgt tgt])))))

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
    (when (record? v)
      v)))

(defn transform-el
  [pth p-el-i to]
  (let [{:as el   p-el-k :el-k} (get pth p-el-i)
        {:as el-1 el-1-k :el-k} (p-el-prev pth p-el-i)
        p-el' (case p-el-k
                :L (let [tgt (-> el :el-argv peek)]
                     (case to
                       :S (data/elemv :S [tgt tgt])
                       :Q (data/elemv :Q [tgt tgt])
                       :C (data/elemv :C [(-> el-1 :el-argv peek) tgt tgt])))
                :C (let [{[c1 c2 tgt] :el-argv} el]
                     (case to
                       :S (data/elemv :S [c2 tgt])
                       :Q (data/elemv :Q [c2 tgt])
                       :L (data/elemv :L [tgt])))
                :S (let [{[c tgt] :el-argv} el]
                     (case to
                       :L (data/elemv :L [tgt])
                       :Q (data/elemv :Q [c tgt])
                       :C (data/elemv :C [(-> el-1 :el-argv peek) c tgt])))
                :Q (let [{[c tgt] :el-argv} el]
                     (case to
                       :L (data/elemv :L [tgt])
                       :S (data/elemv :S [c tgt])
                       :C (data/elemv :C [(-> el-1 :el-argv peek) c tgt]))))]
    (-> pth
        (u/vec-replace p-el-i p-el'))))

;; --- cmpt

(defn append-pth
  [script pi]
  (-> script
      (u/vec-append pi
                    (data/elemv :path [(data/elemv :M [[10 10]])]))))

(defn del-pth [script pi] (-> script (u/vec-remove pi)))

(defn translate
  ([cmpt sel n] (-> cmpt (data-ops/update-s-el-sel sel data-ops/translate-p-els n)))
  ([cmpt     n] (-> cmpt (data-ops/update-s-els        data-ops/translate-p-els n))))

(defn rotate
  ([cmpt sel c a] (-> cmpt (data-ops/update-s-el-sel sel data-ops/rotate-p-els c a)))
  ([cmpt     c a] (-> cmpt (data-ops/update-s-els        data-ops/rotate-p-els c a))))

(defn scale
  ([cmpt sel c n] (-> cmpt (data-ops/update-s-el-sel sel data-ops/scale-p-els c n)))
  ([cmpt     c n] (-> cmpt (data-ops/update-s-els        data-ops/scale-p-els c n))))

(defn absolute
  ([cmpt sel] (-> cmpt (data-ops/update-s-el-sel sel data-ops/normalize-pcmd-seq :op :rel->abs)))
  ([cmpt]     (-> cmpt (data-ops/update-s-els        data-ops/normalize-pcmd-seq :op :rel->abs))))

(defn full
  ([cmpt sel] (-> cmpt (data-ops/update-s-el-sel sel data-ops/normalize-pcmd-seq :op :short->full)))
  ([cmpt]     (-> cmpt (data-ops/update-s-els        data-ops/normalize-pcmd-seq :op :short->full))))

(defn normalize
  ([cmpt sel] (-> cmpt (data-ops/update-s-el-sel sel data-ops/normalize-pcmd-seq :op :all)))
  ([cmpt]     (-> cmpt (data-ops/update-s-els        data-ops/normalize-pcmd-seq :op :all))))

(defn mirror
  ([cmpt axis pos sel] (-> cmpt (data-ops/update-s-el-sel sel #(data-ops/mirror-p-els axis pos %))))
  ([cmpt axis pos]     (-> cmpt (data-ops/update-s-els        #(data-ops/mirror-p-els axis pos %)))))

(defn reverse-path
  ([cmpt sel] (-> cmpt (data-ops/update-s-el-sel sel data-ops/reverse-p-els)))
  ([cmpt]     (-> cmpt (data-ops/update-s-els        data-ops/reverse-p-els))))

(defn update-p-opts [cmpt navr-sel f & args]
  (-> cmpt
      (nav/update-in-nav* (-> navr-sel
                              (assoc :p-el-i 1)) :p-el-i
                          (fn [pth-el]
                            (apply f pth-el args)))))

(defn toggle-d [cmpt navr-sel]
  (-> cmpt
      (nav/update-in-nav* navr-sel :x-el-k
                          (fn [{:keys [el-argv], {:as el-opts :keys [d]} :el-opts}]
                            (cond
                              d     (data/elem :el-k    :path
                                               :el-opts (-> el-opts (dissoc :d))
                                               :el-argv (conv/path-d->els d))
                              :else (data/elem :el-k    :path
                                               :el-opts (-> el-opts
                                                            (assoc :d (paint/path-str cmpt el-opts el-argv)))))))))
