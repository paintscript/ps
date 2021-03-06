(ns paintscript.app.canvas
  (:require [clojure.string :as str]

            [svg-hiccup-kit.core :as sk :refer [d d2 tf tf*]]
            [svg-hiccup-kit.pattern :as sk-pat]
            [reagent.core :as r]

            [paintscript.util :as u]
            [paintscript.nav :as nav]

            [paintscript.data :as data]
            [paintscript.ops.ops-elem :as ops-elem]
            [paintscript.ops.ops-path :as ops-path]
            [paintscript.ops.ops-path-tf :as ops-path-tf]

            [paintscript.render :as render]
            [paintscript.data :as data]

            [paintscript.paint :as paint]
            [paintscript.app.s-app :as s-app]))

(defn coord
  "notes:
   - subject to the same tf-params as the main render
     - this is desirable for translations and scaling of translations
     - but the scaling needs to be reversed for the sizing of the controls"
  [{:as opts :keys [coord-size scaled controls?
                    report-down!
                    report-over!
                    navr-hov
                    navr-sel]}
   els
   {:as el  :keys [el-k]}
   {:as pnt :keys [xy pnt-i-main]}
   locr-ctx]
  (r/with-let [!hover? (r/atom false)]
    (let [navr-hov (-> navr-hov (assoc :ref-pth nil :cmpt-pth0 nil))
          navr-sel (-> navr-sel (assoc :ref-pth nil :cmpt-pth0 nil))
          navr-ctx (-> locr-ctx nav/locr->nav)]
      (when
        (vector? xy) ;; skip v/V, h/H
        (let [[x y :as xy*] xy

              cp?       (some? pnt-i-main)

              sel-pv?   (and (= (:x-el-k navr-sel) (:x-el-k navr-ctx))
                             (= (:p-el-i navr-sel) (:p-el-i navr-ctx)))
              hover?    (= navr-hov navr-ctx)
              sel-pnt?  (= navr-sel navr-ctx)
              tf-params (list [:tl [x y]]
                              [:sc (/ 1 scaled)])]

          (when (or (not cp?) sel-pv?)
            [:g
             (when cp?
               (let [el-main     (-> els (get pnt-i-main) :el-argv peek)
                     [x1 y1]     xy*
                     [x2 y2]     el-main
                     line-params {:x1 x1 :x2 x2
                                  :y1 y1 :y2 y2}]
                 [:g
                  [:line.ctrl-target.under line-params]
                  [:line.ctrl-target.over  line-params]]))

             [:g {:class         (str (if pnt-i-main "control" "target")
                                      (when hover? " hover")
                                      (when sel-pnt? " selected"))
                  :style         {:cursor      "pointer"
                                  :text-select "none"}
                  :on-mouse-down #(report-down! navr-ctx pnt-i-main (-> % .-shiftKey))
                  :on-mouse-over #(report-over! navr-ctx true)
                  :on-mouse-out  #(report-over! navr-ctx false)}

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
                             (str/join " " xy)])]))]]]))))))


(defn plot-coords [opts p-els pnts-seq]

  (for [[el pnts]      pnts-seq
        {:as pnt
         :keys [locr]} (->> pnts
                            ;; NOTE: render CPs last
                            (sort-by :pnt-i-main))
        :let [_ (assert locr (pr-str pnts-seq))]]
    ^{:key (hash locr)}
    [coord opts p-els el pnt locr]))

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
   out-tups
   cmpt-sel-recs]
  (r/with-let [!svg-dom    (if full-screen?
                             (get-in @!s-app [:ui :!full-svg])
                             (atom nil))
               !navr-hov   (r/cursor !s-app [:ui :navr-hov])
               !navr-sel   (r/cursor !s-app [:ui :navr-sel])

               wh0         (get-in cmpt-base [:canvas :dims])

               !svg-params (if full-screen?
                             (r/cursor !s-app [:ui :full-svg-params])
                             (delay {:scale      scale
                                     :canvas-wh  (->> wh0 (mapv #(* % scale)))
                                     :canvas-xy0 [0 0]}))

               dnd-fns     (when c-app
                             (derive-dnd-fns !s-app !svg-dom canvas))

               ref-fn      #(when (and % (not= % @!svg-dom))
                              (reset! !svg-dom %)
                              (when full-screen?
                                (let [on-resize! @(r/cursor !s-app [:ui :on-resize!])]
                                 (on-resize! canvas))))]

    (let [{:as svg-params
           :keys
           [scale
            canvas-xy0]
           [w h :as
            canvas-wh]
           :canvas-wh} @!svg-params

          svg-attrs    (u/deep-merge (if full-screen?
                                       {:class "full-screen"}
                                       {:width w
                                        :height h})
                                     {:ref ref-fn}
                                     (get-in cmpt-base [:canvas :attrs])
                                     dnd-fns)

          tf-params0   {:tl (->> (case zero
                                   :init   [0 0]
                                   :center [(/ w 2) (/ h 2)])
                                 (mapv + canvas-xy0))
                        :sc [scale
                             scale]}

          navr-hov      @!navr-hov
          navr-sel      @!navr-sel

          tf-params    (if-let [rr (:ref-pth navr-sel)]
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
           [tf {:tl canvas-xy0}
            [:image (derive-bg-img-attrs cmpt-base bg scale [w h])]])

         ;; --- background hatching
         [:defs
          [:pattern#diagonalHatch
           {:width  5 :pattern-units "userSpaceOnUse"
            :height 5}
           [:path {:style {:stroke         "var(--blue-light)"
                           :stroke-linecap "square"
                           :stroke-width   1}
                   :d "M 0 5 L 5 0"}]]]

         (when (:hatching canvas)
           [tf {:tl canvas-xy0}
            [:rect {:width  w :fill "url(#diagonalHatch)"
                    :height h}]])

         [tf* tf-params

          ;; --- main

          (when-let [script-pre (:script/pre cmpt-base)]
            [:g.main.pre
             [paint/paint c-fns (-> cmpt-sel
                                    (assoc :script script-pre))]])

          [:g.main
           [paint/paint c-fns cmpt-sel]]

          ;; --- selected segment

          (when (and navr-sel
                     (:p-el-i navr-sel))
            (let [x-el      (nav/get-in-nav cmpt-root navr-sel :x-el-k)
                  p-els'    (-> (case (:src-k navr-sel)
                                  :defs   (-> x-el
                                              ops-path-tf/normalize-pcmds)
                                  :script (-> x-el
                                              ;; to render an individual el it needs to be full & abs:
                                              (ops-elem/update-el-argv ops-path-tf/normalize-pcmds)
                                              :el-argv)))
                  p-els-seg (ops-path-tf/get-path-segment (:src-k  navr-sel) p-els'
                                                          (:p-el-i navr-sel))]
              [:g.sel
               [paint/path-builder c-fns nil {} (:x-el-k navr-sel) p-els-seg]]))

          ;; --- coord circles

          (when coords?
            [:g.coords
             (for [[s-el-i
                    {:as s-el
                     :keys [locr el-k]
                     s-el-opts :el-opts
                     p-els     :el-argv}] out-tups

                   :when (and (not (:disabled? s-el-opts))
                              (or (not (:variant-active cmpt-base))
                                  (not (:variant-key s-el-opts))
                                  (= (:variant-active cmpt-base) (:variant-key s-el-opts))
                                  (= (:variant-active cmpt-base) (:variant-key s-el-opts))))]

               (let [p-els'        (->> p-els
                                        (ops-elem/resolve-els-refs (:defs cmpt-sel))
                                        (#(ops-path-tf/normalize-pcmds % :op :rel->abs)))
                     p-el-pnts-seq (render/path-pnts {:interactive? true
                                                      :coords? true}
                                                     s-el-opts
                                                     p-els')]
                 ^{:key (hash locr)}
                 [:g.coords-plot
                  (plot-coords {:scaled       scale
                                :coord-size   10
                                :report-down! report-down!
                                :report-over! report-over!
                                :navr-sel     navr-sel
                                :navr-hov     navr-hov
                                :controls?    (= (:x-el-k navr-sel)
                                                 (data/get-locr locr :x-el-k))}
                               p-els'
                               p-el-pnts-seq)]))])]]
        (catch :default err
          (println :paint-exec-error)
          (js/console.log err)
          (dispatch! [:op.s-log/undo]))))))

(defn canvas-paint
  ([cmpt-root]
   ;; NOTE: used via gallery
   (canvas-paint nil (r/atom nil) cmpt-root cmpt-root cmpt-root))
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
                        (fn [s-el-i
                             s-el]
                          (case (:el-k s-el)
                            :ref nil ;; TODO: add :ref support
                            [s-el-i s-el])))
                       (remove nil?))

         full-svg-scale @(r/cursor !s-app [:ui :full-svg-scale])]


     [:div.paint
      (for [canvas-inst  (or (when-not full-screen?
                               instances)
                             [nil])]
        (let [cmpt-base' (-> cmpt-base
                             (cond-> canvas-inst
                                     (u/deep-merge canvas-inst)))]
          ^{:key (str (hash canvas-inst)
                      (hash (get cmpt-base' :canvas))
                      (get-in cmpt-base' [:canvas :scale])
                      full-svg-scale)}
          [canvas-paint-variant c-app !s-app variant-active cmpt-root cmpt-base' cmpt-sel

           out-tups
           cmpt-sel]))])))
