(ns paintscript.render-svg
  (:require [clojure.string :as str]

            [svg-hiccup-kit.core :as shk :refer [d d2 tf tf*]]
            #?(:cljs [reagent.core :as r :refer [atom]])

            [paintscript.util :as u]
            [paintscript.el-path :as el-path]
            [paintscript.els :as els]
            [paintscript.nav :as nav]

            [paintscript.render :as render]))

(declare paint)

(def svg-renderer
  (reify render/Renderer
    (els->out [_ els]      (el-path/els->out els))
    (group    [_ els]      (vec (cons :g els)))
    (group    [_ opts els] (vec (concat [:g opts] els)))
    (tf       [_ opts el]  (shk/tf opts el))
    (tf*      [_ opts els] (apply shk/tf* opts els))
    (paint    [_ ps]       (paint ps))
    (paint*   [_ ps-out _ _] ps-out)))

(declare plot-coords)

(defn path-builder
  ([opts els] (path-builder nil opts 0 els))
  ([{:as s-opts :keys [debug?]}
    {:as p-opts :keys [attrs]}
    pi
    els]
   (if-let [d (:d p-opts)]
     (let [d' (-> d (cond-> (el-path/ref? d)
                            (->> (els/resolve-d-ref (:defs s-opts)))))
           {:keys [translate]
            {scale-factor :factor} :scale} p-opts

           pth [:path (merge attrs {:d d'})]]
       (if (or translate scale-factor)
         [tf {:tl translate
              :sc (some-> scale-factor vector)} pth]
         pth))
     (if debug?
       (let [pnts-seq (render/path-pnts s-opts p-opts els)]
         #?(:cljs (plot-coords p-opts pi els pnts-seq)))
       (let [out-seq (render/path svg-renderer s-opts p-opts els)]
         [:path (merge attrs {:d (apply d out-seq)})])))))

(defn- scale->tl [canvas-dims {:keys [factor center]}]
  (let [ratio      (mapv / center canvas-dims)
        dims-delta (mapv #(-> % (* (- factor 1))) canvas-dims)
        tl         (mapv #(-> %1 (* %2) -) dims-delta ratio)]
    tl))

(defn- margin-side [margin side-k]
  (get margin (case side-k
                :left  (case (count margin)
                         2 1
                         4 3)
                :right 1)))

(defn- layout-builder
  [{:as script-opts :keys [defs]}
   obj-opts' els]
  [:g (->> els
           (reduce (fn [[out offset] el]
                     (assert (el-path/ref? el))
                     (let [{:as painting, {:keys [dims]} :canvas}
                           (els/resolve-p-ref defs el)

                           {:keys [margin translate]
                            {sc-ctr :center
                             sc-fct :factor :as scale} :scale}
                           (els/get-opts el)

                           out+
                           ^{:key (hash [offset el])}
                           [tf {:tl (-> [offset 0]
                                        (cond->  margin    (update 0 + (margin-side margin :left)))
                                        (cond->> translate (mapv + translate))
                                        (cond->> scale     (mapv + (scale->tl (get-in painting [:canvas :dims]) scale))))
                                :sc (when scale
                                      [sc-fct])}
                            (paint (merge script-opts painting))]

                           offset+ (-> (first dims)
                                       (cond-> translate (+ (first translate))
                                               scale     (* sc-fct)
                                               margin    (+ (margin-side margin :left)
                                                            (margin-side margin :right))))]
                       [(conj out out+)
                        (+ offset offset+)]))
                   [nil 0])
           first)])

(defn paint
  [{:as script-opts :keys [variant defs styles attrs script data?]}]
  [:g
   (for [[pi [obj-k {:as obj-opts :keys [disabled? variant-k class-k]} & els :as obj]]
         (map-indexed vector script)
         :when (and (not disabled?)
                    (or (not variant)
                        (not variant-k)
                        (= variant variant-k)))
         :let [styles-attrs (if class-k
                              (get styles class-k
                                   (get styles "outline"))
                              (get styles "outline"))
               obj-opts'    (-> obj-opts
                                (->> (merge (select-keys styles-attrs
                                                         [:scale :translate])))
                                (update :attrs merge
                                        attrs
                                        (dissoc styles-attrs
                                                :scale :translate))
                                (dissoc :disabled?))]]

     (with-meta
       (case obj-k
         :path   (path-builder   script-opts obj-opts' pi els)
         :layout (layout-builder script-opts obj-opts' els)

         (-> obj
             (update 1 #(-> %
                            (dissoc :class-k
                                    :variant-k
                                    :disabled?)
                            (merge attrs
                                   styles-attrs)))))
       {:key pi}))])


;; -----------------------------------------------------------------------------
;; web

#?(:cljs
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
         (let [[x y]     (->> (if (el-path/relative? el-k)
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
                                  (cond-> (el-path/relative? el-k)
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
                   :on-mouse-down #(report!       ii-pnt i-main (-> % .-shiftKey))
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

                   [:circle {:cx x :cy y :r coord-size}]))]]))))))

#?(:cljs
   (defn plot-coords [opts pi els pnts-seq]
     (for [[el pnts] pnts-seq
           {:as pnt :keys [xy ii-pnt i-main]} (->> pnts
                                                   ;; NOTE: render CPs last
                                                   (sort-by :i-main))
           :let [_ (assert ii-pnt)]]
       ^{:key (hash ii-pnt)}
       [coord opts els el pnt ii-pnt])))

#?(:cljs
   (defn canvas-paint
     ([config cmpt] (canvas-paint nil config cmpt))
     ([[hov sel dispatch! report! report-hov! set-ref! dnd-fns]
       config cmpt]
      (let [{:keys [variant]
             {:as   canvas
              :keys [variants]} :canvas} cmpt

            [src-k-sel
             pi-sel
             eli-sel
             xyi-sel] sel

            out-tups (->> (:script cmpt)
                          (map-indexed
                           (fn [pi [_k p-opts & els :as path]]
                             [pi p-opts els])))]
        [:div.paint
         (for [variant (or variants
                           (some-> variant list)
                           [nil])
               :let [{:as cmpt
                      {:as   canvas
                       :keys [scale dims coords?]
                       :or   {dims [100 100] coords? true}} :canvas}
                     (-> cmpt
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
                  [paint (u/merge-configs config
                                          variant)]])

               [:g.main
                [paint cmpt]]

               ;; --- selected segment

               (when (and sel (or (and (= :defs src-k-sel)
                                       (get sel 2))
                                  (> eli-sel nav/eli0)))
                 (let [els'    (-> (nav/cmpt> cmpt :src-k src-k-sel :pi pi-sel)
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
                                  (or (not (:variant cmpt))
                                      (not variant-k)
                                      (= (:variant cmpt) variant-k)
                                      (= (:variant cmpt) variant-k)))]
                   (let [els'        (->> els
                                          (els/resolve-els-refs (:defs cmpt))
                                          (els/attach-xy-abs-meta))
                         el-pnts-seq (render/path-pnts {:debug? true
                                                        :coords? true}
                                                       p-opts
                                                       els')]
                     ^{:key pi}
                     [:g.coords-plot
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
               (dispatch! [:undo]))))]))))
