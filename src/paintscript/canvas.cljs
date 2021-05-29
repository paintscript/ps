(ns paintscript.canvas
  (:require [clojure.string :as str]

            [svg-hiccup-kit.core :as sk :refer [d d2 tf tf*]]
            [svg-hiccup-kit.pattern :as sk-pat]
            [reagent.core :as r]

            [paintscript.util :as u]
            [paintscript.el-path :as el-path]
            [paintscript.els :as els]
            [paintscript.nav :as nav]

            [paintscript.render :as render]

            [paintscript.paint :as paint]
            [paintscript.app.s-app :as s-app]))

(defn coord
  "notes:
   - subject to the same tf-params as the main render
     - this is desirable for translations and scaling of translations
     - but the scaling needs to be reversed for the sizing of the controls"
  [{:as opts :keys [scaled report-down! report-over! coord-size controls? hov-rec sel-rec]}
   els
   {:as el  :keys [p-el-k]}
   {:as pnt :keys [xy xy-abs i-main]}
   pth-rec]
  (r/with-let [!hover? (r/atom false)]
    (when
      (vector? xy) ;; skip v/V, h/H
      (let [
            [x y :as xy*] (or xy-abs
                              xy)

            cp?       (some? i-main)

            sel-pv?   (and (= (:x-el-k sel-rec) (:x-el-k pth-rec))
                           (= (:p-el-i sel-rec) (:p-el-i pth-rec)))
            hover?    (= hov-rec pth-rec)
            sel-pnt?  (= sel-rec pth-rec)
            tf-params (list [:tl [x y]]
                            [:sc (/ 1 scaled)])]

        (when (or (not cp?) sel-pv?)
          [:g
           (when cp?
             (let [[x1 y1] xy*
                   [x2 y2] (-> els
                               (nth i-main)
                               last
                               (cond-> (el-path/relative? p-el-k)
                                       (#(-> % els/xy-abs-meta (or %)))))
                   line-params {:x1 x1 :x2 x2
                                :y1 y1 :y2 y2}]
               [:g
                [:line.ctrl-target.under line-params]
                [:line.ctrl-target.over  line-params]]))

           [:g {:class         (str (if i-main "control" "target")
                                    (when hover? " hover")
                                    (when sel-pnt? " selected"))
                :style         {:cursor      "pointer"
                                :text-select "none"}
                :on-mouse-down #(report-down! pth-rec i-main (-> % .-shiftKey))
                :on-mouse-over #(report-over! pth-rec true)
                :on-mouse-out  #(report-over! pth-rec false)}

            [tf tf-params
             (cond
               cp?   [:g.cp
                      [:rect {:x      (- (/ coord-size 2))
                              :y      (- (/ coord-size 2))
                              :width  coord-size
                              :height coord-size}]]
               :else (let [big? (or sel-pnt? hover?)]
                       [:g
                        [:circle {:r (-> coord-size (cond-> big? (* 1.8)))}]
                        (when (and big?
                                   (not (or sel-pnt? cp?)))
                          [:text {:fill              "white"
                                  :font-size         coord-size
                                  :text-anchor       "middle"
                                  :dominant-baseline "middle"
                                  :style             {:user-select "none"}}
                           (str/join " " xy)])]))]]])))))


(defn plot-coords [opts p-els pnts-seq]
  (for [[el pnts] pnts-seq
        {:as pnt :keys [xy pth-rec i-main]} (->> pnts
                                                 ;; NOTE: render CPs last
                                                 (sort-by :i-main))
        :let [_ (assert pth-rec)]]
    ^{:key (hash pth-rec)}
    [coord opts p-els el pnt pth-rec]))

(defn- derive-bg-img-attrs
  [{:as   cmpt-base :keys [canvas]}
   {:as   bg
    :keys [align url scale translate opacity]
    :or   {scale 1 translate [0 0] align :center opacity 0.5}
    wh-img :size}
   canvas-scale
   wh-svg]
  (let [[w-img
         h-img
         :as wh-img'] (cond
                        wh-img
                        (let [scale* (* scale
                                        (or canvas-scale 1))]
                          (mapv * [scale*
                                   scale*] wh-img))

                        :else
                        (mapv * [scale
                                 scale] wh-svg))

        [x y] (->> translate
                   (mapv #(* % (or canvas-scale 1))) ;; scale the translation
                   ((fn [xy]
                      (case align
                        :init   xy
                        :center (let [offset (->> (mapv - wh-img' wh-svg)
                                                  (mapv #(-> % (/ 2))))]
                                  (mapv - xy offset))))))]

    {:href   url
     :width  w-img
     :height h-img
     :style  {:opacity   opacity
              :transform (str "translate(" x "px," y "px)")}}))

(def ^:private c-fns {:plot-coords plot-coords})

(defn- canvas-paint-variant
  "notes:
   - in non-interactive contexts (e.g. gallery) `c-app` can be omitted"
  [{:as c-app :keys [dispatch! report-down! report-over! derive-dnd-fns]}
   !s-app
   variant-active

   cmpt-root
   {:as cmpt-base
    {:as   canvas
     :keys [full-screen? scale dims coords? zero]
     :or   {dims    [100 100]
            zero    :init
            coords? true}} :canvas}
   cmpt-sel
   out-tups]
  (r/with-let [!svg-dom        (if full-screen?
                                 (get-in @!s-app [:ui :!full-svg])
                                 (atom nil))
               !full-scale     (r/cursor !s-app [:ui :full-scale])
               !svg-dims       (r/cursor !s-app [:ui :full-svg-dims])
               !ui             (r/cursor !s-app [:ui])
               !hov-rec        (r/cursor !s-app [:ui :hov-rec])
               !sel-rec        (r/cursor !s-app [:ui :sel-rec])
               [w0 h0 :as wh0] (get-in cmpt-base [:canvas :dims])

               dnd-fns   (when c-app
                           (derive-dnd-fns !s-app !svg-dom canvas))]
    (let [scale        (s-app/derive-scale !s-app canvas)
          [w h :as wh] (->> wh0
                            (mapv #(* % scale)))
          xy-shift  (if-not full-screen?
                      [0 0]
                      ;; NOTE: rounding creates sharper hatching lines
                      (mapv #(-> %1 (- %2) (/ 2) Math/round) @!svg-dims wh))
          hov-rec   @!hov-rec
          sel-rec   @!sel-rec
          svg-attrs (u/deep-merge (if full-screen?
                                    {:class "full-screen"}
                                    {:width w
                                     :height h})
                                  {:ref #(when (and % (not @!svg-dom))
                                           (reset! !svg-dom %)
                                           (let [f (get-in @!s-app [:ui :on-resize!])]
                                             (f)))}
                                  (get-in cmpt-base [:canvas :attrs])
                                  dnd-fns)
          tf-params0 {:tl (->> (case zero
                                 :init   [0 0]
                                 :center [(/ w 2) (/ h 2)])
                               (mapv + xy-shift))
                      :sc [scale
                           scale]}

          tf-params  (if-let [rr (:ref-pth sel-rec)]
                       ;; NOTE: doesn't support :repeat yet
                       (concat tf-params0
                               (->> rr
                                    (mapcat paint/normalize-tf-params)))
                       tf-params0)]

      (try
        ^{:key (hash variant-active)}
        [:svg svg-attrs

         ;; --- background image
         (when-let [bg (get-in cmpt-base [:canvas :background])]
           [tf {:tl xy-shift}
            [:image (derive-bg-img-attrs cmpt-base bg scale [w h])]])

         ;; --- background hatching
         [:defs
          [:pattern#diagonalHatch
           {:pattern-units "userSpaceOnUse" :width 5 :height 5}
           [:path {:style {:stroke         "var(--blue-light)"
                           :stroke-linecap "square"
                           :stroke-width   1}
                   :d "M 0 5 L 5 0"}]]]

         (when (:hatching canvas)
           [tf {:tl xy-shift}
            [:rect {:width w :height h :fill "url(#diagonalHatch)"}]])

         [tf* tf-params

          ;; --- main

          (when-let [script-pre (:script/pre cmpt-base)]
            [:g.main.pre
             [paint/paint c-fns (-> cmpt-sel
                                    (assoc :script script-pre))]])

          [:g.main
           [paint/paint c-fns cmpt-sel]]

          ;; --- selected segment

          (when (and sel-rec
                     (or (and (= :defs (:src-k sel-rec))
                              (:p-el-i sel-rec))
                         (> (:p-el-i sel-rec) nav/p-el-i0)))
            (let [p-els'    (-> (nav/cmpt> cmpt-sel
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
               [paint/path-builder c-fns nil {} (:x-el-k sel-rec) p-els-seg]]))

          ;; --- coord circles

          (when coords?
            [:g.coords
             (for [[s-el-i
                    {:as s-el-opts :keys [variant-key]} p-els] out-tups
                   :when (and (not (:disabled? s-el-opts))
                              (or (not (:variant-active cmpt-base))
                                  (not variant-key)
                                  (= (:variant-active cmpt-base) variant-key)
                                  (= (:variant-active cmpt-base) variant-key)))]

               (let [p-els'        (->> p-els
                                        (els/resolve-els-refs (:defs cmpt-sel))
                                        (els/attach-xy-abs-meta))
                     p-el-pnts-seq (render/path-pnts {:interactive? true
                                                      :coords? true}
                                                     s-el-opts
                                                     p-els')]
                 ^{:key s-el-i}
                 [:g.coords-plot
                  (plot-coords {:scaled       scale
                                :coord-size   10
                                :report-down! report-down!
                                :report-over! report-over!
                                :sel-rec      sel-rec
                                :hov-rec      hov-rec
                                :controls?    (= (:x-el-k sel-rec)
                                                 s-el-i)}
                               p-els'
                               p-el-pnts-seq)]))])]]
        (catch :default err
          (println :paint-exec-error)
          (js/console.log err)
          (dispatch! [:undo]))))))

(defn canvas-paint
  ([cmpt-root]
   ;; NOTE: used via gallery
   (canvas-paint nil nil cmpt-root cmpt-root cmpt-root))
  ([c-app !s-app
    cmpt-root cmpt-base cmpt-sel]
   (let [;; NOTE: can't be cached b/c floats hash to integer
         ; config (u/deep-merge config-external
         ;                      (:config cmpt))

         {:keys [variant-active]
          {:as   canvas
           :keys [full-screen?
                  instances]} :canvas} cmpt-base

         out-tups (->> (:script cmpt-sel)
                       (map-indexed
                        (fn [s-el-i [s-el-k
                                     s-el-opts & p-els]]
                          (case s-el-k
                            :ref nil ;; TODO: add :ref support
                            [s-el-i s-el-opts p-els])))
                       (remove nil?))]


     [:div.paint
      (for [canvas-inst  (or (when-not full-screen?
                               instances)
                             [nil])]
        (let [cmpt-base' (-> cmpt-base
                             (cond-> canvas-inst
                                     (u/deep-merge canvas-inst)))]
          ^{:key (str (hash canvas-inst)
                      (hash (get cmpt-base' :canvas))
                      (get-in cmpt-base' [:canvas :scale]))}
          [canvas-paint-variant c-app !s-app variant-active cmpt-root cmpt-base' cmpt-sel out-tups]))])))
