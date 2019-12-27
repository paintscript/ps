(ns paintscript.core
  (:require #?(:cljs [reagent.core :as r :refer [atom]])
            [clojure.string :as str]
            [svg-hiccup-kit.core :refer [d d2]]
            [paintscript.util :as u]))

(defn- arcs
  [pth {:as opts :keys [mode ctd?] :or {mode :concave}}]
  (let [[head & tail] pth
        paired-with-prev (map vector tail (drop-last pth))
        arc-middle (str (case mode :concave "0,1" :convex "0,0") )]
    (concat (when-not ctd?
              [["M" head]])
            (for [[[x  y  :as pnt]
                   [xp yp :as pnt-prev]] paired-with-prev]
              ["A" [(- x xp) (- y yp)] [0 arc-middle] [x y]]))))

(defn- xy-mouse [ev]
  [(-> ev .-clientX)
   (-> ev .-clientY)])

(defn- round [n]
  #?(:cljs (js/Math.round n)))

(defn- parse-vec [[k & args]]
  (if (map? (first args))
    [k (first args) 2 (rest args)]
    [k nil 1 args]))

(defn- flip-c2 [c2 tgt]
  (let [delta (mapv - tgt c2)]
    (mapv + tgt delta)))

(defn- flip-bin [n] (case n 0 1 0))

(defn- mirror-pnt  [width pnt]  (update pnt 0 #(- width %)))
(defn- mirror-pnts [width pnts] (map #(mirror-pnt width %) pnts))

(defn- mirror-pth-vecs [width pth-vv]
  (->> pth-vv
       (map (fn [[pth-k & pnts :as pth-v]]
              (case pth-k
                :arc     (let [[opts & pnts] pnts]
                           (concat [pth-k (-> opts
                                              (update :mode #(case % :convex :concave :convex)))]
                                   (mirror-pnts width pnts)))
                :curve-A (let [[r [rot f1 f2] xy] pnts

                               ;; NOTE: needs to be undone when combined with reverse
                               f2' (-> f2 flip-bin)]
                           [:curve-A r [rot f1 f2'] (mirror-pnt width xy)])
                (cons pth-k
                      (mirror-pnts width pnts)))))))

(defn- normalize-curves [pth-vv]
  (loop [[[pth-k & pnts :as pth-v] & pth-vv-tail] pth-vv
         tgt-prev nil
         c2-prev  nil
         acc      []]
    (if-not pth-k
      acc
      (case pth-k
        :line     (recur pth-vv-tail (last pth-v) nil (conj acc pth-v))
        :line*    (recur pth-vv-tail (last pth-v) nil (conj acc pth-v))
        :arc      (let [[arg1 & args-rest] pnts
                        arc' (if (map? arg1) pth-v (concat [:arc {}] pnts))]
                    (recur pth-vv-tail (last pth-v) nil (conj acc arc')))

        :curve-A  (let [[_ _ tgt] pnts]
                    (recur pth-vv-tail tgt nil (conj acc pth-v)))

        :curve-C  (let [[_ c2 tgt] pnts]
                    (recur pth-vv-tail tgt c2 (conj acc pth-v)))

        :curve-c  (let [[c1 c2 tgt :as pnts'] (map #(mapv + % tgt-prev) pnts)]
                    (recur pth-vv-tail tgt c2 (conj acc [:curve-C c1 c2 tgt])))

        :curve-C1 (let [[c1 tgt] pnts
                        curve-C [:curve-C c1 tgt tgt]]
                    (recur pth-vv-tail tgt nil (conj acc curve-C)))

        :curve-S  (let [[c2 tgt] pnts
                        c1 (flip-c2 c2-prev tgt-prev)
                        curve-C [:curve-C c1 c2 tgt]]
                    (recur pth-vv-tail tgt c2 (conj acc curve-C)))

        :curve-s  (let [[c2 tgt] (map #(mapv + % tgt-prev) pnts)
                        c1 (flip-c2 c2-prev tgt-prev)
                        curve-C [:curve-C c1 c2 tgt]]
                    (recur pth-vv-tail tgt c2 (conj acc curve-C)))

        :curve-Q  (let [[_ tgt] pnts]
                    (recur pth-vv-tail tgt nil (conj acc pth-v)))))))

(defn- steal-next [pth-vv-tail]
  (let [xy-1         (-> pth-vv-tail first last)
        pth-v-next   (-> pth-vv-tail first drop-last)
        pth-vv-tail' (-> pth-vv-tail rest (conj pth-v-next))]
    [xy-1 pth-vv-tail']))

(defn- reverse-pth-vec-pnts
  "drop last point and redistribute the rest in reverse:
    ([:line 1 2] [:curve-C 3 4 5] [:curve-C 6 7 8])
    => ([:curve-C 7 6 5] [:curve-C 4 3 2] [:line* 1])
  "
  [pth-vv]
  (loop [[[pth-k & pnts :as pth-v-curr] & pth-vv-tail] (reverse pth-vv)
         acc []]
    (if-not pth-v-curr
      acc
      (case pth-k
        :line    (let [i (cons :line* (reverse pnts))]
                   (recur pth-vv-tail (-> acc (cond-> (seq pnts) (conj i)))))

        :line*   (let [[xy-1
                        pth-vv-tail'] (steal-next pth-vv-tail)
                       i (cons :line* (concat (reverse pnts) [xy-1]))]
                   (recur pth-vv-tail' (-> acc (cond-> (seq pnts) (conj i)))))

        :arc     (let [i (cons :arc* (reverse pnts))]
                   (recur pth-vv-tail (-> acc (cond-> (seq pnts) (conj i)))))

        :curve-A  (let [[r p xy2] pnts
                        p' (update p 2 flip-bin)
                        [xy-1
                         pth-vv-tail'] (steal-next pth-vv-tail)
                        i              [:curve-A r p' xy-1]]
                    (recur pth-vv-tail' (conj acc i)))

        :curve-C (let [[c1 c2 xy]     pnts
                       [xy-1
                        pth-vv-tail'] (steal-next pth-vv-tail)
                       i              [:curve-C c2 c1 xy-1]]
                   (recur pth-vv-tail' (conj acc i)))

        :curve-Q (let [[c xy] pnts
                       [xy-1
                        pth-vv-tail'] (steal-next pth-vv-tail)
                       i              [:curve-Q c xy-1]]
                   (recur pth-vv-tail' (conj acc i)))))))

(defn- reverse-pth-vecs
  [width pth-vecs]
  (->> pth-vecs
       normalize-curves
       reverse-pth-vec-pnts))

(defn map-pnts [f pth-vecs]
  (->> pth-vecs
       (map (fn [[op-k & pnts :as pth-vec]]
              (case op-k
                :curve-A (-> pth-vec (update 3 f))
                (cons op-k
                      (map f pnts)))))))

(defn scale-path [pth-vecs ctr n]
  (map-pnts #(u/tl-point-towards % ctr n) pth-vecs))

(defn pth-vec-->svg-seq [i pth-vec]
  (let [[k opts i-o args] (parse-vec pth-vec)
        data [args i i-o]]
    (case k
      :arc      [data (arcs args opts)]
      :arc*     [data (arcs args (assoc opts :ctd? true))]
      :line     [data (cons "M" args)]
      :line*    [data (cons "L" args)]
      :curve-A  (let [[r  p  tgt] args] [data (list "A" r  p   tgt)])
      :curve-S  (let [[   c2 tgt] args] [data (list "S"    c2  tgt)])
      :curve-s  (let [[   c2 tgt] args] [data (list "s"    c2  tgt)])
      :curve-C  (let [[c1 c2 tgt] args] [data (list "C" c1 c2  tgt)])
      :curve-c  (let [[c1 c2 tgt] args] [data (list "c" c1 c2  tgt)])
      :curve-C1 (let [[c1    tgt] args] [data (list "C" c1 tgt tgt)])
      :curve-c1 (let [[c1    tgt] args] [data (list "c" c1 tgt tgt)])
      :curve-Q  (let [[c     tgt] args] [data (list "Q" c      tgt)])
      :curve-q  (let [[c     tgt] args] [data (list "q" c      tgt)])
      :curve-T  (let [[c1    tgt] args] [data (list "T" c1 tgt tgt)])
      :curve-t  (let [[c1    tgt] args] [data (list "t" c1 tgt tgt)]))))

(defn path
  ([pth-vecs] (path nil pth-vecs))
  ([{:keys [mode debug? close? cutout? draw? width mirror]
     [scale-ctr scale-fract :as scale] :scale
     :or   {width 100
            mode  :concave}}
    pth-vecs-init]
   (let
     [!pth-vecs (atom pth-vecs-init)]
     (let [pth-vecs (-> @!pth-vecs
                        (cond-> scale
                                (scale-path scale-ctr scale-fract)))
           pnt-tups
           (for [[i pth-vec] (map-indexed vector pth-vecs)]
             (pth-vec-->svg-seq i pth-vec))

           points
           (-> pnt-tups
               (->> (map second))
               (cond-> mirror
                       (as-> pnts
                             (->> (case mirror
                                    nil       pth-vecs
                                    :merged   (->> pth-vecs (reverse-pth-vecs width))
                                    :separate (->> pth-vecs normalize-curves))
                                  (mirror-pth-vecs width)
                                  (path {:debug? true})
                                  first
                                  (map second)
                                  (concat pnts))))
               (cond-> close? (concat ["Z"]))
               flatten)]

       (if debug?
         [pnt-tups points]
         (if draw?
           [:path {:d (apply d points)}]
           points))))))

;; -----------------------------------------------------------------------------
;; UI

(defn coord [scaled xy-init upd! coord-size]
  (#?(:cljs r/with-let :clj let)
    [!xy   (atom xy-init)
     !snap (atom nil)]
    (let [[x y] @!xy]
      [:g {:style {:cursor "pointer"
                   :text-select "none"}
           :on-mouse-down #(reset! !snap {:xy0 [x y] :m0 (xy-mouse %)})
           :on-mouse-move (fn [ev]
                            (when-let [{:as snap :keys [xy0 m0]} @!snap]
                              (let [m1  (xy-mouse ev)
                                    d   (mapv - m1 m0)
                                    d'  (mapv / d scaled)
                                    d'  (mapv round d')
                                    xy' (mapv + xy0 d')]
                                (upd! xy')
                                (reset! !xy xy'))))
           :on-mouse-up   #(reset! !snap nil)}
       [:circle {:cx x :cy y :r (-> 0.2 (* coord-size)) :fill "hsla(0,100%,50%,1)"}]
       [:circle {:cx x :cy y :r (-> 1.5 (* coord-size)) :fill "hsla(240,100%,50%,0.3)"}]
       [:text {:x x :y (+ y 0.5) :fill "white"
               :font-size (-> 1 (* coord-size))
               :text-anchor "middle"
               :style {:user-select "none"}}
        (str x " " y)]])))

(defn path-builder [{:as opts :keys [scaled debug? attrs coord-size]
                     :or {scaled [1 1]}}
                    pth-vecs-init]
  (#?(:cljs r/with-let :clj let)
    [!pth-vecs (atom pth-vecs-init)]
    (let [pth-vecs @!pth-vecs
          [pnt-tups points] (path (assoc opts :debug? true) pth-vecs)]
      [:g
       [:path (merge attrs
                     {:d (apply d points)})]
       (when debug?
         (for [[args i pnt-offset] (map first pnt-tups)
               [i-pnt pnt] (map-indexed vector args)
               :let [upd! #(swap! !pth-vecs assoc-in [i (+ i-pnt pnt-offset)] %)]]
           ^{:key (hash [i i-pnt])}
           [coord scaled pnt upd! (or coord-size 1)]))])))
