(ns paintscript.ops)

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

(defn has-pred? [pth-i] (pos? pth-i))

(defn el-len [el]
  (-> el count dec))

(defn get-tgt [el]
  (last el))

(defn offset-pnt [pnt delta] (mapv + pnt delta))
(defn xy-delta  [pnt1 pnt2] (mapv - pnt1 pnt2))

(defn get-pnt [els [pth-i pnt-i-abs :as ii]] (get-in els ii))

(defn get-pred-tgt [els pth-i]
  (-> (get els (-> pth-i (- 1))) get-tgt))

;; points

(def pnt-i-offset 1)
(def el-i-offset 2)

(defn infer-succ-pnt
  [els pth-i pnt-i]
  (let [el (get els pth-i)
        pnt-i'  (+ pnt-i pnt-i-offset)
        pnt-0   (get-pnt els [pth-i pnt-i'])]
    (case (el-len el)
      0 (if (has-pred? pth-i)
          (offset-pnt (get-pred-tgt els pth-i)
                      [10 10])
          [0 0])

      1 (if (has-pred? pth-i)
          (let [pnt-1 (-> (get els (-> pth-i (- 1))) get-tgt)]
            (offset-pnt pnt-0 (xy-delta pnt-0 pnt-1)))
          (offset-pnt pnt-0 (xy-delta pnt-0 [0 0])))

      (let [pnt-1 (get-pnt els [pth-i (-> pnt-i' (- 1))])]
        (offset-pnt pnt-0
                    (xy-delta pnt-0 pnt-1))))))

(defn append-pnt
  ([els pth-i]
   (append-pnt els pth-i (-> (get els pth-i) el-len dec)))

  ;; infer successor
  ([els pth-i pnt-i]
   (append-pnt els pth-i pnt-i
               (infer-succ-pnt els pth-i pnt-i)))

  ;; explicit:
  ([els pth-i pnt-i pnt]
   (let [pnt-i' (+ pnt-i pnt-i-offset)]
     (-> els
         (update pth-i vec-append pnt-i' pnt)))))

(defn del-pnt
  [els pth-i pnt-i]
  (let [pnt-i' (+ pnt-i pnt-i-offset)]
    (-> els
        (update pth-i vec-remove pnt-i'))))

;; els

(defn infer-succ-el [els pth-i]
  (let [[k & pnts :as el-1] (get els pth-i)]
    (case k
      :M [:L (offset-pnt (last pnts) [10 10])]
      :L [:L (offset-pnt (last pnts) [10 10])]
      :C (let [tgt (offset-pnt (last pnts) [10 10])]
           [:C (last pnts) tgt tgt])
      :S (let [tgt (offset-pnt (last pnts) [10 10])]
           [:S tgt tgt]))))

(defn append-el
  [els pth-i]
  (-> els
      (vec-append pth-i (infer-succ-el els pth-i))))

(defn del-el
  [els pth-i]
  (-> els
      (vec-remove pth-i)))

;; paths

(defn append-pth
  [script pth-i]
  (-> script
      (vec-append pth-i [:path {} [:M [10 10]]])))

(defn del-pth [script pth-i] (-> script (vec-remove pth-i)))
