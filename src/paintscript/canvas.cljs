(ns paintscript.canvas
  (:require [clojure.string :as str]

            [svg-hiccup-kit.core :as shk :refer [d d2 tf tf*]]
            [reagent.core :as r]

            [paintscript.util :as u]
            [paintscript.el-path :as el-path]
            [paintscript.els :as els]
            [paintscript.nav :as nav]

            [paintscript.render :as render]

            [paintscript.paint :refer [paint
                                       path-builder]]))

(defn coord
  [{:as opts :keys [scaled report-down! report-over! coord-size controls? hov-rec sel-rec]}
   els
   {:as el  :keys [p-el-k]}
   {:as pnt :keys [xy xy-abs i-main]}
   pth-rec]
  (r/with-let [!hover? (r/atom false)]
    (when
      (vector? xy) ;; skip v/V, h/H
      (let [[x y]     (->> (if (el-path/relative? p-el-k)
                             xy-abs
                             xy)
                           (mapv #(* % scaled)))
            cp?       (some? i-main)

            sel-pv?   (and (= (:x-el-k sel-rec)
                              (:x-el-k pth-rec))
                           (= (:p-el-i sel-rec)
                              (:p-el-i pth-rec)))
            pth-rec*  pth-rec
            ; pth-rec*  (nav/pth-vec->rec ii-pnt)
            hover?    (= hov-rec pth-rec*)
            sel-pnt?  (= sel-rec pth-rec*)]

        (when (or (not cp?) sel-pv?)
          [:g
           (when cp?
             (let [[x2 y2] (-> els
                               (nth i-main)
                               last
                               (cond-> (el-path/relative? p-el-k)
                                       (#(-> % els/xy-abs-meta (or %))))
                               (->> (mapv #(* % scaled))))]
               [:g
                [:line.ctrl-target.under {:x1 x :y1 y :x2 x2 :y2 y2}]
                [:line.ctrl-target.over  {:x1 x :y1 y :x2 x2 :y2 y2}]]))

           [:g {:class         (str (if i-main "control" "target")
                                    (when hover? " hover")
                                    (when sel-pnt? " selected"))
                :style         {:cursor      "pointer"
                                :text-select "none"}
                :on-mouse-down #(report-down! pth-rec* i-main (-> % .-shiftKey))
                :on-mouse-over #(report-over! pth-rec* true)
                :on-mouse-out  #(report-over! pth-rec* false)}

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
        {:as pnt :keys [xy pth-rec i-main]} (->> pnts
                                                 ;; NOTE: render CPs last
                                                 (sort-by :i-main))
        :let [_ (assert pth-rec)]]
    ^{:key (hash pth-rec)}
    [coord opts els el pnt pth-rec]))

(defn- derive-background-image-attrs
  [{:as config :keys [canvas]}
   {:keys [align url scale translate opacity]
    :or {scale 1 translate [0 0] align :center opacity 0.5}
    wh-img :size}
   wh-svg]
  (let [[w-img
         h-img
         :as wh-img'] (cond
                        wh-img
                        (let [scale* (* scale
                                        (or (:scale canvas) 1))]
                          (mapv * [scale*
                                   scale*] wh-img))

                        :else
                        (mapv * [scale
                                 scale] wh-svg))

        [x y] (->> translate
                   (mapv #(* % (or (:scale canvas) 1))) ;; scale the translation
                   ((fn [xy]
                      (case align
                        :init   xy
                        :center (let [offset (->> (mapv - wh-img' wh-svg)
                                                  (mapv #(-> % (/ 2))))]
                                  (mapv - xy offset))))))]

    {:href url
     :width w-img
     :height h-img
     :style {:opacity opacity
             :transform (str "translate(" x "px," y "px)")}}))

(def ^:private c-fns {:plot-coords plot-coords})

(defn- canvas-paint-variant
  [[hov-rec sel-rec dispatch! report-down! report-over! derive-dnd-fns :as c-app]
   config variant-active
   {:as cmpt
    {:as   canvas
     :keys [scale dims coords? zero]
     :or   {dims [100 100]
            zero :init
            coords? true}} :canvas}
   out-tups]
  (r/with-let [!svg-dom (atom nil)
               [w h]    (->> dims (mapv #(* % scale)))
               dnd-fns  (derive-dnd-fns !svg-dom scale)]
    (let [config*  (u/deep-merge config
                                 (:config cmpt))
          cmpt*    (u/deep-merge config*
                                 cmpt)
          view-box (case zero
                     :init nil
                     :center (str (/ w -2) " " (/ h -2) " " w " " h))]
      (try
        ^{:key (hash variant-active)}
        [:svg (u/deep-merge {:width  w :height h
                             :view-box view-box
                             :ref    #(when (and % (not @!svg-dom))
                                        (reset! !svg-dom %))}
                            (get-in config* [:canvas :attrs])
                            dnd-fns)
         (when-let [bg (get-in config* [:canvas :background])]
           [:image (derive-background-image-attrs cmpt* bg [w h])])
         [:defs
          [:pattern#diagonalHatch
           {:pattern-units "userSpaceOnUse"
            :width  5
            :height 5}
           [:path {:d "M 0 5 L 5 0"
                   :style {:stroke "var(--blue-light)" :stroke-width 1
                           :stroke-linecap "square"}}]]]

         (when (:hatching canvas)
           [:rect {:width w :height h :fill "url(#diagonalHatch)"}])

         [tf* {:sc [scale scale]}

          ;; --- output

          (when-let [script-pre (:script/pre config)]
            [:g.main.pre
             [paint c-fns (-> cmpt*
                              (assoc :script script-pre))]])

          [:g.main
           [paint c-fns cmpt*]]

          ;; --- selected segment

          (when (and sel-rec
                     (or (and (= :defs (:src-k sel-rec))
                              (:p-el-i sel-rec))
                         (> (:p-el-i sel-rec) nav/p-el-i0)))
            (let [p-els'    (-> (nav/cmpt> cmpt
                                           :src-k (:src-k sel-rec)
                                           :s-eli (:x-el-k sel-rec))

                                ;; to render an individual el it needs to be full & abs:
                                (els/update-p-els els/normalize-p-els)

                                (els/extract-p-els (:src-k  sel-rec)
                                                   (:p-el-i sel-rec))
                                vec)
                  p-els-seg (els/get-path-segment (:src-k  sel-rec) p-els'
                                                  (:p-el-i sel-rec))]
              [:g.sel
               [path-builder c-fns nil {} (:x-el-k sel-rec) p-els-seg]]))]

         ;; --- coord circles

         (when coords?
           [:g.coords
            (for [[s-el-i {:as s-el-opts :keys [variant-key]} p-els] out-tups
                  :when (and (not (:disabled? s-el-opts))
                             (or (not (:variant-active cmpt))
                                 (not variant-key)
                                 (= (:variant-active cmpt) variant-key)
                                 (= (:variant-active cmpt) variant-key)))]
              (let [p-els'        (->> p-els
                                       (els/resolve-els-refs (:defs cmpt))
                                       (els/attach-xy-abs-meta))
                    p-el-pnts-seq (render/path-pnts {:debug?  true
                                                     :coords? true}
                                                    s-el-opts
                                                    p-els')]
                ^{:key s-el-i}
                [:g.coords-plot
                 (plot-coords {:scaled        scale
                               :coord-size    10
                               :report-down!  report-down!
                               :report-over!  report-over!
                               :sel-rec       sel-rec
                               :hov-rec       hov-rec
                               :controls?     (= (:x-el-k sel-rec) s-el-i)}
                              s-el-i
                              p-els'
                              p-el-pnts-seq)]))])]
        (catch :default err
          (println :paint-exec-error)
          (js/console.log err)
          (dispatch! [:undo]))))))

(defn canvas-paint
  ([config cmpt] (canvas-paint nil config cmpt))
  ([c-app config cmpt]
   (let [;; NOTE: can't be cached b/c floats hash to integer
         config' (u/deep-merge config
                               (:config cmpt))

         {:keys [variant-active]
          {:as   canvas
           :keys [instances]} :canvas} cmpt

         out-tups (->> (:script cmpt)
                       (map-indexed
                        (fn [s-el-i [s-el-k
                                     s-el-opts & p-els]]
                          (case s-el-k
                            :ref nil ;; TODO: add :ref support
                            [s-el-i s-el-opts p-els])))
                       (remove nil?))]

     [:div.paint
      (for [canvas-inst (or instances
                            [nil])]
        (let [cmpt' (-> cmpt
                        (cond-> (map? canvas-inst)
                                (u/deep-merge canvas-inst)))]
          ^{:key (str (hash canvas-inst)
                      (get-in cmpt' [:canvas :scale]))}
          [canvas-paint-variant c-app config' variant-active cmpt' out-tups]))])))
