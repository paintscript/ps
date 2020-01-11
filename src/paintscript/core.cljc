(ns paintscript.core
  (:require #?(:cljs [reagent.core :as r :refer [atom]])
            [clojure.string :as str]
            [svg-hiccup-kit.core :refer [d d2]]
            [paintscript.els :as els]
            [paintscript.ctrl :as ctrl]))

(def ^:private relative? #{:c :s})

(defn coord [{:keys [scaled report! report-hover! coord-size controls? hov]
              [src-k-sel pi-sel eli-sel xyi-sel :as sel] :sel}
             els el-k el
             xy [src-k pi eli _ :as iii]]
  (#?(:cljs r/with-let :clj let) [!hover? (atom false)]
    (let [[x y] (->> (-> xy (cond-> (relative? el-k) els/abs-meta))
                     (mapv #(* % scaled)))
          i-tgt     (-> xy meta :i-tgt)
          cp?       (some? i-tgt)
          hover?    (= iii hov)
          sel-pv?   (and (= pi-sel  pi)
                         (= eli-sel eli))
          sel-pnt?  (= iii sel)]
      (when (or (not cp?) sel-pv?)
        [:g
         (when cp?
           (let [[x2 y2] (-> els
                             (nth i-tgt)
                             last
                             (cond-> (relative? el-k)
                                     (#(-> % els/abs-meta (or %))))
                             (->> (mapv #(* % scaled))))]
            [:g
             [:line.ctrl-target.under {:x1 x :y1 y :x2 x2 :y2 y2}]
             [:line.ctrl-target.over  {:x1 x :y1 y :x2 x2 :y2 y2}]]))
         [:g {:style {:cursor "pointer" :text-select "none"}
              :on-mouse-down #(report!       iii)
              :on-mouse-over #(report-hover! iii true)
              :on-mouse-out  #(report-hover! iii false)
              :class (str (if i-tgt "control" "target")
                          (when hover? " hover")
                          (when (= sel iii) " selected"))}
          (if cp?
            [:g.cp
             [:rect {:x (- x (/ coord-size 2))
                     :y (- y (/ coord-size 2))
                     :width  coord-size
                     :height coord-size}]]

            (if (or sel-pnt? hover?)
              [:g
               [:circle {:cx x :cy y :r (* coord-size 1.8)}]
               (when-not (or sel-pnt? cp?)
                 [:text {:x x :y y
                         :fill "white"
                         :font-size coord-size
                         :text-anchor "middle"
                         :dominant-baseline "middle"
                         :style {:user-select "none"}}
                  (str/join " " xy)])]

              [:circle {:cx x :cy y :r coord-size}]))]]))))

(defn plot-coords [opts pi els data-svg-tups]
  (for [[args _eli xyi0 el-k el] (map first data-svg-tups)
        [xyi xy] (reverse (map-indexed vector args))
        :let [iii (some-> el meta :ii (concat [(+ xyi xyi0)]))
              _   (assert iii)]]
    ^{:key (hash iii)}
    [coord opts els el-k el xy iii]))

(def path els/path)

(defn path-builder
  ([opts els] (path-builder opts 0 els))
  ([{:as opts :keys [debug? attrs]}
    pi els]
   (let [[data-svg-tups svg-seq] (els/path (assoc opts :debug? true) els)]
     [:g
      (if debug?
        (plot-coords opts pi els data-svg-tups)
        [:path (merge attrs {:d (apply d svg-seq)})])])))

(defn paint [{:as script-opts :keys [variant defs styles attrs script data?]}]
  [:g
   (for [[pi [obj-k {:as p-opts :keys [variant-k class-k]} & els :as obj]]
         (map-indexed vector script)
         :when (or (not variant)
                   (not variant-k)
                   (= variant variant-k))
         :let [styles-attrs (if class-k
                              (get styles class-k
                                   (get styles "outline"))
                              (get styles "outline"))
               p-opts' (-> p-opts
                           (merge script-opts)
                           (update :attrs merge styles-attrs))]]

     (with-meta
       (case obj-k
         :path (path-builder p-opts' pi els)
         (-> obj
             (update 1 #(-> %
                            (dissoc :class-k :variant-k)
                            (merge attrs
                                   styles-attrs)))))
       {:key pi}))])
