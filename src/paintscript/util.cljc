(ns paintscript.util)

(defn round  [n] #?(:cljs (js/Math.round n) :clj n))
(defn round1 [n] #?(:cljs (-> n (+ js/Number.EPSILON) (* 10) js/Math.round (/ 10))
                    :clj  n))
(defn round2 [n] #?(:cljs (-> n (+ js/Number.EPSILON) (* 100) js/Math.round (/ 100))
                    :clj  n))

(defn sign [n] (if (neg? n) -1 1))

(def PI #?(:cljs js/Math.PI :clj Math/PI))

(defn rad [deg] (* deg (/ PI 180)))
(defn deg [rad] (* rad (/ 180 PI)))

(def pow  #?(:cljs js/Math.pow  :clj #(Math/pow %1 %2)))
(def sqrt #?(:cljs js/Math.sqrt :clj #(Math/sqrt %)))
(def cos  #?(:cljs js/Math.cos  :clj #(Math/cos %)))
(def sin  #?(:cljs js/Math.sin  :clj #(Math/sin %)))
(def atan #?(:cljs js/Math.atan :clj #(Math/atan %)))

(def cos'  (comp cos rad))
(def sin'  (comp sin rad))
(def atan' (comp deg atan))

(defn slope [[x1 y1] [x2 y2]]
  (/ (- y2 y1)
     (- x2 x1)))

(defn dist [[x1 y1] [x2 y2]]
  (sqrt (+ (pow (- y2 y1) 2)
           (pow (- x2 x1) 2))))

(def v+ (partial mapv (comp round2 +)))

(defn tl-point
  ([[x y] [dx dy]]
   [(+ x dx) (+ y dy)])
  ([[x y] a l]
   (let [x' (+ x (* l (cos' a)))
         y' (+ y (* l (sin' a)))]
     [x' y'])))

(defn angle-between [[x1 y1 :as xy1] [x2 y2 :as xy2]]
  (if (= x1 x2)
    (-> 90 (cond-> (neg? (- y2 y1)) -))
    (let [slope (slope xy1 xy2)]
      (atan' slope))))

(defn tl-point-towards [pnt ctr factor]
  (let [factor   (- factor)
        [dx dy] (mapv - ctr pnt)
        dist    (dist ctr pnt)
        dist-mv (-> dist
                    (- (- 1 (* dist factor)))
                    (* (sign dx)))]
    (tl-point pnt
              (angle-between pnt ctr)
              dist-mv)))

(defn map-vals [f coll]
  (into {} (map (fn [[k v]] [k (f v)])) coll))

(defn merge-maps [& args]
  (if (every? map? args) (apply merge args) (last args)))

(defn merge-configs [& args]
  (apply merge-with merge-maps args))
