(ns paintscript.util)

;; --- colls

(defn map-vals [f coll]
  (into {} (map (fn [[k v]] [k (f v)])) coll))

(defn merge-maps [& args]
  (if (every? map? args) (apply merge args) (last args)))

(defn merge-configs [& args]
  (apply merge-with merge-maps args))

;; --- vecs

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

;; --- numbers

(defn round  [n] #?(:cljs (js/Math.round n) :clj n))
(defn round1 [n] #?(:cljs (-> n (+ js/Number.EPSILON) (* 10)  js/Math.round (/ 10))
                    :clj  n))
(defn round2 [n] #?(:cljs (-> n (+ js/Number.EPSILON) (* 100) js/Math.round (/ 100))
                    :clj  n))

(defn sign [n] (if (neg? n) -1 1))

;; --- trigonometry

(def PI #?(:clj Math/PI :cljs js/Math.PI))

(defn rad [deg] (* deg (/ PI 180)))
(defn deg [rad] (* rad (/ 180 PI)))

(def pow  #(#?(:clj Math/pow  :cljs js/Math.pow)  %1 %2))
(def sqrt #(#?(:clj Math/sqrt :cljs js/Math.sqrt) %))
(def cos  #(#?(:clj Math/cos  :cljs js/Math.cos)  %))
(def sin  #(#?(:clj Math/sin  :cljs js/Math.sin)  %))
(def atan #(#?(:clj Math/atan :cljs js/Math.atan) %))

(def cos'  (comp cos rad))
(def sin'  (comp sin rad))
(def atan' (comp deg atan))

(defn slope [[x1 y1] [x2 y2]]
  (let [denom (- x2 x1)]
    (when-not (zero? denom)
      (/ (- y2 y1)
         denom))))

(defn dist [[x1 y1] [x2 y2]]
  (sqrt (+ (pow (- y2 y1) 2)
           (pow (- x2 x1) 2))))

;; --- tl

(def v+ (partial mapv (comp round2 +)))

(defn tl-point
  [[x y] [dx dy]]
  [(+ x dx)
   (+ y dy)])

;; --- tl-point-towards

(defn- tl-point-along
  [[x y] a l]
  (let [x' (+ x (* l (cos' a)))
        y' (+ y (* l (sin' a)))]
    [x' y']))

(defn- angle-delta
  [[x1 y1 :as xy1]
   [x2 y2 :as xy2]]
  (if (= x1 x2)
    (-> 90 (cond-> (neg? (- y2 y1)) -))
    (let [slope (slope xy1 xy2)]
      (atan' slope))))

(defn tl-point-towards [pnt ctr factor]
  ;; TODO: rewrite ITO angle-between
  (let [factor  (- factor)
        [dx dy] (mapv - ctr pnt)
        dist    (dist ctr pnt)
        dist-mv (-> dist
                    (- (- 1 (* dist factor)))
                    (* (sign dx)))]
    (tl-point-along pnt
                    (angle-delta pnt ctr)
                    dist-mv)))

;; --- other

(defn angle-between [[x1 y1 :as xy1]
                     [x2 y2 :as xy2]]
  (cond
    (= x1 x2) (if (> y2 y1) 270 90)
    (= y1 y2) (if (> x2 x1) 0  180)
    :else     (let [sl (slope xy1 xy2)]
                (-> sl - atan'
                    (cond-> (or (and (pos? sl) (> x1 x2))
                                (and (neg? sl) (< x2 x1)))
                            (+ 180))))))

(def rad-between (comp rad angle-between))

(defn- point-on-ellipse
  "divide the ellipse into `n-segs` and return the position of `i-seg`"
  [[rx ry :as ellipse] offset n-segs i-seg]
  (let [rad (-> PI
                (- (* 2 offset))
                (-)
                (/ (dec n-segs))
                (*  i-seg)
                (- offset))
        x   (* rx (cos rad))
        y   (* ry (sin rad))]
    [x y]))

(defn- point-at-angle
  ([radius alpha]
   [(* radius (cos' (- alpha)))
    (* radius (sin' (- alpha)))])
  ([center radius alpha]
   (mapv #(round1 (+ %1 %2)) center (point-at-angle radius alpha))))

(defn tl-point-around [ctr pnt alphad]
  (let [alpha0 (angle-between ctr pnt)
        radius (dist ctr pnt)]
    (point-at-angle ctr radius (+ alpha0
                                  alphad))))
