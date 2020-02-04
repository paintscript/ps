(ns paintscript.canvas
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [cljs.pprint :refer [pprint]]
            [cljs.reader :refer [read-string]]
            [reagent.core :as r]
            [z-com.core :as zc]
            [svg-hiccup-kit.core :refer [tf tf* d d2]]
            [keybind.core :as key]

            [paintscript.ops :as ops]
            [paintscript.els :as els]
            [paintscript.core :as ps]
            [paintscript.nav :as nav]
            [paintscript.ctrl :as ctrl]))

(defn- pprint' [edn] (with-out-str *out* (pprint edn)))

(defn get-path-segment [src-k-sel els eli]
  (let [el-prev    (nav/els-prev els (case src-k-sel :defs :eln :eli) eli)
        [k :as el] (nav/els>     els (case src-k-sel :defs :eln :eli) eli)]
    (concat
     (when (and el-prev
                (not= :M (first el)))
       (list [:M (last el-prev)]))
     (list el))))

(defn canvas [params-init cfg-init]
  (r/with-let [ui-init      {:sel nil :snap nil}
               !ui          (r/atom ui-init)
               !sel         (r/cursor !ui [:sel])
               !config      (r/atom cfg-init)
               !scale       (r/cursor !config [:canvas :scale])
               !params      (r/atom params-init)
               !state-log   (r/atom (list {:params params-init
                                           :ui     ui-init}))
               !hov         (r/atom nil)
               !tab         (r/atom :script)
               !shell       (r/atom "")

               report!      (fn [iii] (reset! !sel iii))
               dispatch!    (partial ctrl/dispatch! !params !state-log !ui)
               dnd-fns      (ctrl/drag-and-drop-fns !scale !params !ui dispatch!)
               kb-fns       (ctrl/keybind-fns              !params !ui dispatch!)
               report-hov!  (fn [iii val]
                              (swap! !hov #(cond
                                             val iii
                                             (= iii %) nil
                                             :else %)))

               _ (doseq [[k f] kb-fns]
                   (key/bind! k (keyword k) f))

               set-ref! #(when (and % (not (:xy-svg @!ui)))
                           (let [rect   (-> % (.getBoundingClientRect))
                                 xy-svg [(-> rect .-left)
                                         (-> rect .-top)]]
                             (println :xy-svg xy-svg)
                             (swap! !ui assoc :xy-svg xy-svg)))]

    (let [config     @!config
          params     @!params

          {:keys [sel xy-svg]} @!ui

          {:as   params'
           :keys [variant]
           {:as   canvas
            :keys [variants]} :canvas}
          (-> (merge-with merge
                          config
                          params)
              (update :script els/attach-iii-meta*))

          tab @!tab

          [src-k-sel
           pi-sel
           eli-sel
           xyi-sel] sel

          hov @!hov

          out-tups (->> (:script params')
                        (map-indexed
                         (fn [pi [_ p-opts & els :as path]]
                           (let [p-opts' (-> p-opts
                                             (assoc :defs (:defs params')))]
                             [pi p-opts' els
                              (ps/path (merge p-opts' {:debug? true})
                                       els)]))))]
      [:div.canvas
       [:div.sidebar.script-phantom]
       [:div.sidebar.script {:class (when sel "with-status")}
        [:div.controls
         (for [tab-k [:script :config]]
           ^{:key tab-k}
           [zc/button
            :label    (name tab-k)
            :active?  (= tab-k tab)
            :on-click #(reset! !tab tab-k)])
         [zc/button
          :label     "undo"
          :disabled? (= 1 (count @!state-log))
          :on-click  #(dispatch! [:undo])]]

        (case tab
          :script [:textarea
                   {:value     (str/trim (pprint' params))
                    :on-focus  #(key/disable!)
                    :on-blur   #(key/enable!)
                    :on-change #(reset! !params (-> % .-target .-value read-string))}]
          :config [:textarea
                   {:value     (str/trim (pprint' config))
                    :on-focus  #(key/disable!)
                    :on-blur   #(key/enable!)
                    :on-change #(reset! !config (-> % .-target .-value read-string))}])

        [:div.shell
         [:textarea
          {:value       @!shell
           :placeholder "enter command..."
           :on-focus    #(key/disable!)
           :on-blur     #(key/enable!)
           :on-change   #(reset! !shell (-> % .-target .-value))
           :on-key-down (fn [e]
                          (let [k (max (.-keyCode e)
                                       (.-which e))
                                cmd @!shell]
                            (when (= 13 k)
                              (reset! !shell "")
                              (dispatch! [:cmd (str/trim cmd)])
                              (-> e (.preventDefault)))))}]]
        [:div.status
         (when sel
           (let [[_ opts :as p] (nav/params> params' :src-k src-k-sel :pi  pi-sel)
                 [k & xys]      (nav/p>      p       :eli   eli-sel)]
             [:div.selection-stack
              [:div.selection-level.iii
               [:span (pr-str sel)]
               [:div.controls.crud
                [zc/button :label "blur" :on-click #(dispatch! [:sel nil])]
                [zc/button :label "up" :on-click #(dispatch! [:sel (drop-last sel)])]]]

              [:div.selection-level.path
               [:span (pr-str opts)]
               [:div.controls.crud
                [zc/button :label "add" :on-click #(dispatch! [:pth-append])]
                [zc/button :label "del" :on-click #(dispatch! [:pth-del])]]]

              [:div.selection-level.path-vec
               [:span.pth-k (pr-str k)]
               [:div.controls.crud
                [zc/button :label "add" :on-click #(dispatch! [:el-append])]
                [zc/button :label "del" :on-click #(dispatch! [:el-del])]]]

              [:div.selection-level.point
               [:span (pr-str (nav/xys> xys :xyi xyi-sel))]
               (when (contains? #{:L :arc} k)
                 [:div.controls.crud
                  [zc/button :label "add" :on-click #(dispatch! [:xy-append])]
                  [zc/button :label "del" :on-click #(dispatch! [:xy-del])]])]]))]]

       [:div.paint
        (for [variant (or variants
                          (some-> variant list)
                          [nil])
              :let [{:as params'
                     {:as   canvas
                      :keys [scale dims coords?]
                      :or   {dims [100 100] coords? true}} :canvas}
                    (-> params'
                        (cond-> (keyword? variant) (assoc :variant variant)
                                (map?     variant) (-> (assoc :variant (:variant variant))
                                                       (update :canvas merge variant))))

                    [w h] (->> dims (mapv #(* % scale)))]]

          ^{:key (hash variant)}
          [:svg (merge-with merge
                            {:style {:width w :height h}
                             :ref   set-ref!}
                            (get-in config [:canvas :attrs])
                            dnd-fns)
           [tf* {:sc [scale scale]}

            [:g.main
             [ps/paint params']]

            (when (and sel (or (and (= :defs src-k-sel)
                                    (get sel 2))
                               (> eli-sel nav/eli0)))
              (let [els' (->> (nav/params> params' :src-k src-k-sel :pi pi-sel)
                              (take (inc eli-sel))
                              (drop (if (= :defs src-k-sel) 0 nav/eli0))
                              els/normalize-els)
                    els-seg (get-path-segment src-k-sel els' eli-sel)]
                [:g.sel
                 [ps/path-builder {} pi-sel els-seg]]))]

           (when coords?
             [:g.coords
              (for [[pi {:as p-opts :keys [variant-k]} els _] out-tups
                    :when (and (not (:disabled? p-opts))
                               (or (not (:variant params'))
                                   (not variant-k)
                                   (= (:variant params') variant-k)
                                   (= (:variant params') variant-k)))]
                (let [els'           (->> els
                                          (els/resolve-refs (:defs p-opts))
                                          (els/attach-normalized-meta))
                      [data-svg-tups
                       _svg-seq]     (ps/path (merge p-opts {:debug? true
                                                             :coords? true})
                                              els')]
                  ^{:key pi}
                  [:g
                   (ps/plot-coords {:scaled        scale
                                    :coord-size    10
                                    :report!       report!
                                    :report-hover! report-hov!
                                    :sel           sel
                                    :hov           hov
                                    :controls?     (= pi-sel pi)}
                                   pi els' data-svg-tups)]))])])]])))
