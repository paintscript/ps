(ns paintscript.canvas
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [cljs.pprint :refer [pprint]]
            [cljs.reader :refer [read-string]]

            [reagent.core :as r]
            [z-com.core :as zc]
            [svg-hiccup-kit.core :refer [tf tf* d d2]]
            [keybind.core :as key]

            [paintscript.util :as u]
            [paintscript.ops :as ops]
            [paintscript.els :as els]
            [paintscript.core :as ps]
            [paintscript.nav :as nav]
            [paintscript.ctrl :as ctrl]))

(defn- pprint' [edn] (with-out-str *out* (pprint edn)))

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
                        (fn [pi [_ p-opts & els :as path]]
                          (let [pth (ps/path {:defs (:defs params)
                                              :debug? true}
                                             p-opts
                                             els)]
                            [pi p-opts els pth]))))]
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

            (when (:script config)
              [:g.main
               [ps/paint (u/merge-configs
                          config
                          variant)]])

            [:g.main
             [ps/paint params]]

            (when (and sel (or (and (= :defs src-k-sel)
                                    (get sel 2))
                               (> eli-sel nav/eli0)))
              (let [els' (->> (nav/params> params :src-k src-k-sel :pi pi-sel)
                              (take (inc eli-sel))
                              (drop (if (= :defs src-k-sel) 0 nav/eli0))
                              els/normalize-els)
                    els-seg (els/get-path-segment src-k-sel els' eli-sel)]
                [:g.sel
                 [ps/path-builder nil {} pi-sel els-seg]]))]

           (when coords?
             [:g.coords
              (for [[pi {:as p-opts :keys [variant-k]} els _] out-tups
                    :when (and (not (:disabled? p-opts))
                               (or (not (:variant params))
                                   (not variant-k)
                                   (= (:variant params) variant-k)
                                   (= (:variant params) variant-k)))]
                (let [els'           (->> els
                                          (els/resolve-els-refs (:defs params))
                                          (els/attach-normalized-meta))
                      [data-svg-tups
                       _svg-seq]     (ps/path {:debug? true
                                               :coords? true}
                                              p-opts
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
                                   pi els' data-svg-tups)]))])]
          (catch :default e
            (println :paint-exec-error)
            (js/console.log e)
            (dispatch! [:undo]))))])))

(defn- canvas-sidebar
  [!config !params !shell !state-log !tab
   config params params' sel
   dispatch!]
  (let [tab @!tab
        [src-k-sel pi-sel eli-sel xyi-sel] sel]
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
               [zc/button :label "del" :on-click #(dispatch! [:xy-del])]])]]))]]))

(defn canvas [cfg-init params-init]
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
                             (swap! !ui assoc :xy-svg xy-svg)))

               canvas-paint' (with-meta #'canvas-paint
                               {:component-did-catch
                                (fn [e info]
                                  (println :paint-error e)
                                  (js/console.log e)
                                  (dispatch! [:undo]))})]

    (let [config     @!config
          params     @!params

          {:keys [sel]} @!ui

          params'
          (-> (merge-with merge
                          (-> config (dissoc :script))
                          params)
              (update :script els/attach-iii-meta*))]

      [:div.canvas
       [:div.sidebar.script-phantom]
       [canvas-sidebar
        !config !params !shell !state-log !tab
        config params params' sel dispatch!]

       [canvas-paint'
        [@!hov sel dispatch! report! report-hov! set-ref! dnd-fns]
        config params']])))
