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

(defn pth-vec-len [pth-vec]
  (-> pth-vec count dec))

(defn get-tgt [pth-vec]
  (last pth-vec))

(defn offset-pnt [pnt delta] (mapv + pnt delta))
(defn pnt-delta  [pnt1 pnt2] (mapv - pnt1 pnt2))

(defn get-pnt [pth-vecs [pth-i pnt-i-abs :as ii]] (get-in pth-vecs ii))

(defn get-pred-tgt [pth-vecs pth-i]
  (-> (get pth-vecs (-> pth-i (- 1))) get-tgt))

;; points

(def pnt-i-offset 1)
(def pth-vec-i-offset 2)

(defn infer-succ-pnt
  [pth-vecs pth-i pnt-i]
  (let [pth-vec (get pth-vecs pth-i)
        pnt-i'  (+ pnt-i pnt-i-offset)
        pnt-0   (get-pnt pth-vecs [pth-i pnt-i'])]
    (case (pth-vec-len pth-vec)
      0 (if (has-pred? pth-i)
          (offset-pnt (get-pred-tgt pth-vecs pth-i)
                      [10 10])
          [0 0])

      1 (if (has-pred? pth-i)
          (let [pnt-1 (-> (get pth-vecs (-> pth-i (- 1))) get-tgt)]
            (offset-pnt pnt-0 (pnt-delta pnt-0 pnt-1)))
          (offset-pnt pnt-0 (pnt-delta pnt-0 [0 0])))

      (let [pnt-1 (get-pnt pth-vecs [pth-i (-> pnt-i' (- 1))])]
        (offset-pnt pnt-0
                    (pnt-delta pnt-0 pnt-1))))))

(defn append-pnt
  ([pth-vecs pth-i]
   (append-pnt pth-vecs pth-i (-> (get pth-vecs pth-i) pth-vec-len dec)))

  ;; infer successor
  ([pth-vecs pth-i pnt-i]
   (append-pnt pth-vecs pth-i pnt-i
               (infer-succ-pnt pth-vecs pth-i pnt-i)))

  ;; explicit:
  ([pth-vecs pth-i pnt-i pnt]
   (let [pnt-i' (+ pnt-i pnt-i-offset)]
     (-> pth-vecs
         (update pth-i vec-append pnt-i' pnt)))))

(defn del-pnt
  [pth-vecs pth-i pnt-i]
  (let [pnt-i' (+ pnt-i pnt-i-offset)]
    (-> pth-vecs
        (update pth-i vec-remove pnt-i'))))

;; path-vecs

(defn infer-succ-pth-vec [pth-vecs pth-i]
  (let [[k & pnts :as pth-vec-1] (get pth-vecs pth-i)]
    (case k
      :M [:L (offset-pnt (last pnts) [10 10])]
      :L [:L (offset-pnt (last pnts) [10 10])]
      :C (let [tgt (offset-pnt (last pnts) [10 10])]
           [:C (last pnts) tgt tgt])
      :S (let [tgt (offset-pnt (last pnts) [10 10])]
           [:S tgt tgt]))))

(defn append-pth-vec
  [pth-vecs pth-i]
  (-> pth-vecs
      (vec-append pth-i (infer-succ-pth-vec pth-vecs pth-i))))

(defn del-pth-vec
  [pth-vecs pth-i]
  (-> pth-vecs
      (vec-remove pth-i)))
