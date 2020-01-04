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

(defn get-pnt [els [pi xyi-abs :as ii]] (get-in els ii))

(defn get-pred-tgt [els pi]
  (-> (get els (-> pi (- 1))) get-tgt))

;; points

(def xyi-offset 1)
(def el-i-offset 2)

(defn infer-succ-pnt
  [els pi xyi]
  (let [el (get els pi)
        xyi'  (+ xyi xyi-offset)
        pnt-0   (get-pnt els [pi xyi'])]
    (case (el-len el)
      0 (if (has-pred? pi)
          (offset-pnt (get-pred-tgt els pi)
                      [10 10])
          [0 0])

      1 (if (has-pred? pi)
          (let [pnt-1 (-> (get els (-> pi (- 1))) get-tgt)]
            (offset-pnt pnt-0 (xy-delta pnt-0 pnt-1)))
          (offset-pnt pnt-0 (xy-delta pnt-0 [0 0])))

      (let [pnt-1 (get-pnt els [pi (-> xyi' (- 1))])]
        (offset-pnt pnt-0
                    (xy-delta pnt-0 pnt-1))))))

(defn append-pnt
  ([els pi]
   (append-pnt els pi (-> (get els pi) el-len dec)))

  ;; infer successor
  ([els pi xyi]
   (append-pnt els pi xyi
               (infer-succ-pnt els pi xyi)))

  ;; explicit:
  ([els pi xyi pnt]
   (let [xyi' (+ xyi xyi-offset)]
     (-> els
         (update pi vec-append xyi' pnt)))))

(defn del-pnt
  [els pi xyi]
  (let [xyi' (+ xyi xyi-offset)]
    (-> els
        (update pi vec-remove xyi'))))

;; els

(defn infer-succ-el [els pi]
  (let [[k & pnts :as el-1] (get els pi)]
    (case k
      :M [:L (offset-pnt (last pnts) [10 10])]
      :L [:L (offset-pnt (last pnts) [10 10])]
      :C (let [tgt (offset-pnt (last pnts) [10 10])]
           [:C (last pnts) tgt tgt])
      :S (let [tgt (offset-pnt (last pnts) [10 10])]
           [:S tgt tgt]))))

(defn append-el
  [els pi]
  (-> els
      (vec-append pi (infer-succ-el els pi))))

(defn del-el
  [els pi]
  (-> els
      (vec-remove pi)))

;; lift

(defn update-p-els [p f & args]
  (vec
   (concat (take 2 p)
           (apply f (drop 2 p) args))))

(defn update-px [params [src-k px] f & args]
  (case src-k
    :defs   (apply update-in params [src-k px] f args)
    :script (apply update-in params [src-k px] update-p-els f args)))

;; params

(defn append-pth
  [script pi]
  (-> script
      (vec-append pi [:path {} [:M [10 10]]])))

(defn del-pth [script pi] (-> script (vec-remove pi)))

(defn tl-pth [params ii tl]
  (-> params
      (update-px ii #(els/map-xys (partial u/v+ tl) %))))
