(ns paintscript.render-svg-web
  (:require [clojure.string :as str]

            [svg-hiccup-kit.core :refer [d d2 tf tf*]]
            [reagent.core :as r :refer [atom]]

            [paintscript.util :as u]
            [paintscript.el :as el]
            [paintscript.els :as els]
            [paintscript.nav :as nav]

            [paintscript.render :as render]
            [paintscript.render-svg :refer [svg-renderer]]))

(defn coord
  [{:as opts :keys [scaled report! report-hover! coord-size controls? hov]
    [src-k-sel pi-sel eli-sel xyi-sel :as sel] :sel}
   els
   {:as el  :keys [el-k]}
   {:as pnt :keys [xy xy-abs i-main]}
   [src-k pi eli _ :as ii-pnt]]
  (r/with-let [!hover? (atom false)]
    (when
      (vector? xy) ;; skip v/V, h/H
      (let [[x y]     (->> (if (el/relative? el-k)
                             xy-abs
                             xy)
                           (mapv #(* % scaled)))
            cp?       (some? i-main)
            hover?    (= ii-pnt hov)
            sel-pv?   (and (= pi-sel  pi)
                           (= eli-sel eli))
            sel-pnt?  (= ii-pnt sel)]
        (when (or (not cp?) sel-pv?)
          [:g
           (when cp?
             (let [[x2 y2] (-> els
                               (nth i-main)
                               last
                               (cond-> (el/relative? el-k)
                                       (#(-> % els/xy-abs-meta (or %))))
                               (->> (mapv #(* % scaled))))]
               [:g
                [:line.ctrl-target.under {:x1 x :y1 y :x2 x2 :y2 y2}]
                [:line.ctrl-target.over  {:x1 x :y1 y :x2 x2 :y2 y2}]]))
           [:g {:class         (str (if i-main "control" "target")
                               (when hover? " hover")
                               (when (= sel ii-pnt) " selected"))
                :style         {:cursor      "pointer"
                                :text-select "none"}
                :on-mouse-down #(report!       ii-pnt)
                :on-mouse-over #(report-hover! ii-pnt true)
                :on-mouse-out  #(report-hover! ii-pnt false)}
            (if cp?
              [:g.cp
               [:rect {:x      (-> x (- (/ coord-size 2)))
                       :y      (-> y (- (/ coord-size 2)))
                       :width  coord-size
                       :height coord-size}]]

              (if (or sel-pnt? hover?)
                [:g
                 [:circle {:cx x :cy y :r (* coord-size 1.8)}]
                 (when-not (or sel-pnt? cp?)
                   [:text {:x                 x
                           :y                 y
                           :fill              "white"
                           :font-size         coord-size
                           :text-anchor       "middle"
                           :dominant-baseline "middle"
                           :style             {:user-select "none"}}
                    (str/join " " xy)])]

                [:circle {:cx x :cy y :r coord-size}]))]])))))

(defn plot-coords [opts pi els pnts-seq]
  (for [[el pnts] pnts-seq
        {:as pnt :keys [xy ii-pnt i-main]} pnts
        :let [_ (assert ii-pnt)]]
    ^{:key (hash ii-pnt)}
    [coord opts els el pnt ii-pnt]))

(defn path-builder
  ([opts els] (path-builder nil opts 0 els))
  ([{:as s-opts :keys [debug?]}
    {:as p-opts :keys [attrs]}
    pi
    els]
   (if-let [d (:d p-opts)]
     (let [d' (-> d (cond-> (el/ref? d)
                            (->> (els/resolve-d-ref (:defs s-opts)))))
           {:keys [translate]
            {scale-factor :factor} :scale} p-opts

           pth [:path (merge attrs {:d d'})]]
       (if (or translate scale-factor)
         [tf {:tl translate :sc [scale-factor]} pth]
         pth))
     (if debug?
       (let [pnts-seq (render/path-pnts s-opts p-opts els)]
         (plot-coords p-opts pi els pnts-seq))
       (let [out-seq (render/path svg-renderer s-opts p-opts els)]
         [:path (merge attrs {:d (apply d out-seq)})])))))

(defn paint
  [{:as script-opts :keys [variant defs styles attrs script data?]}]
  [:g
   (for [[pi [obj-k {:as p-opts :keys [disabled? variant-k class-k]} & els :as obj]]
         (map-indexed vector script)
         :when (and (not disabled?)
                    (or (not variant)
                        (not variant-k)
                        (= variant variant-k)))
         :let [styles-attrs (if class-k
                              (get styles class-k
                                   (get styles "outline"))
                              (get styles "outline"))
               p-opts'      (-> p-opts
                                (->> (merge (select-keys styles-attrs
                                                         [:scale :translate])))
                                (update :attrs merge
                                        (dissoc styles-attrs
                                                :scale :translate)))]]

     (with-meta
       (case obj-k
         :path (path-builder script-opts p-opts' pi els)

         (-> obj
             (update 1 #(-> %
                            (dissoc :class-k :variant-k)
                            (merge attrs
                                   styles-attrs)))))
       {:key pi}))])

(defn canvas-paint
  ([config params] (canvas-paint nil config params))
  ([[hov sel dispatch! report! report-hov! set-ref! dnd-fns]
    config params]
   (let [{:keys [variant]
          {:as   canvas
           :keys [variants]} :canvas}
         params

         [src-k-sel
          pi-sel
          eli-sel
          xyi-sel] sel

         out-tups (->> (:script params)
                       (map-indexed
                        (fn [pi [_k p-opts & els :as path]]
                          [pi p-opts els])))]
     [:div.paint
      (for [variant (or variants
                        (some-> variant list)
                        [nil])
            :let [{:as params
                   {:as   canvas
                    :keys [scale dims coords?]
                    :or   {dims [100 100] coords? true}} :canvas}
                  (-> params
                      (cond-> (keyword? variant) (assoc :variant variant)
                              (map?     variant) (u/merge-configs variant)))

                  [w h] (->> dims (mapv #(* % scale)))]]

        (try
          ^{:key (hash variant)}
          [:svg (merge-with merge
                            {:style {:width w :height h}
                             :ref   set-ref!}
                            (get-in config [:canvas :attrs])
                            dnd-fns)
           [tf* {:sc [scale scale]}

            ;; --- output

            (when (:script config)
              [:g.main
               [paint (u/merge-configs
                       config
                       variant)]])

            [:g.main
             [paint params]]

            ;; --- selected segment

            (when (and sel (or (and (= :defs src-k-sel)
                                    (get sel 2))
                               (> eli-sel nav/eli0)))
              (let [els'    (-> (nav/params> params :src-k src-k-sel :pi pi-sel)
                                ;; to render an individual el it needs to be full & abs:
                                (els/update-p-els els/normalize-els)
                                (->> (take (inc eli-sel))
                                     (drop (if (= :defs src-k-sel) 0 nav/eli0)))
                                vec)
                    els-seg (els/get-path-segment src-k-sel els' eli-sel)]
                [:g.sel
                 [path-builder nil {} pi-sel els-seg]]))]

           ;; --- coord circles

           (when coords?
             [:g.coords
              (for [[pi {:as p-opts :keys [variant-k]} els] out-tups
                    :when (and (not (:disabled? p-opts))
                               (or (not (:variant params))
                                   (not variant-k)
                                   (= (:variant params) variant-k)
                                   (= (:variant params) variant-k)))]
                (let [els'        (->> els
                                       (els/resolve-els-refs (:defs params))
                                       (els/attach-xy-abs-meta))
                      el-pnts-seq (render/path-pnts {:debug? true
                                                     :coords? true}
                                                    p-opts
                                                    els')]
                  ^{:key pi}
                  [:g
                   (plot-coords {:scaled        scale
                                 :coord-size    10
                                 :report!       report!
                                 :report-hover! report-hov!
                                 :sel           sel
                                 :hov           hov
                                 :controls?     (= pi-sel pi)}
                                pi els' el-pnts-seq)]))])]
          (catch :default err
            (println :paint-exec-error)
            (js/console.log err)
            (dispatch! [:undo]))))])))
