(ns paintscript.util
  (:require [clojure.string :as str]
            [clojure.pprint :refer [pprint]]

            #?(:cljs [keybind.core :as kb])))

(defn pprint*
  ([edn] (with-out-str *out* (pprint edn)))
  ([title edn]
   (println (str title
                 "\n   "
                 (str/replace (-> edn
                                  (cond-> (not (string? edn))
                                          pprint*))
                              "\n" "\n   ")))
   edn))

;; --- colls

(defn conjv [coll el] (-> (or coll []) (conj el)))

(defn map-vals [f coll]
  (into {} (map (fn [[k v]] [k (f v)])) coll))

(declare deep-merge)

(defn merge-maps [& args]
  (if (every? (some-fn map? nil?) args) (apply deep-merge args) (last args)))

(defn deep-merge [& args]
  (apply merge-with merge-maps args))

(defn cascading-merges [tree pth-seq kk]
  (last
   (reduce (fn [[tree acc] pth]
             (let [tree' (get-in tree pth)]
               [tree'
                (reduce (fn [acc k]
                          (-> acc
                              (update k deep-merge (get tree' k))))
                        acc
                        kk)]))
           [tree {}]
           (cons nil pth-seq))))

; (cascading-merges {:a {:x {"x1" 1} :y {"y1" 2}
;                        :b {:x {"x2" 2} :y {"y2" 2}}}}
;                   (list [:a]
;                         [:b])
;                   [:x :y])

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



; #?(:cljs
;    (defn init-key-bindings! [kbnds]
;      (doseq [[id-k [str-k f]] kbnds]
;        (kb/bind! str-k id-k f))
;      (fn _term-kb! []
;        (doseq [[id-k [str-k f]] kbnds]
;          (kb/unbind! str-k id-k)))))
