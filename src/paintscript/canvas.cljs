(ns paintscript.canvas
  (:require [clojure.string :as str]
            [cljs.pprint :refer [pprint]]
            [cljs.reader :refer [read-string]]

            [reagent.core :as r]
            [z-com.core :as zc]
            [keybind.core :as key]

            [paintscript.els :as els]
            [paintscript.nav :as nav]
            [paintscript.ctrl :as ctrl]

            [paintscript.render-svg-web :as render-svg-web]))

(defn- pprint' [edn] (with-out-str *out* (pprint edn)))

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
  (r/with-let [ui-init      {:sel nil :sel-set nil :snap nil}
               !ui          (r/atom ui-init)
               !sel         (r/cursor !ui [:sel])
               !sel-set     (r/cursor !ui [:sel-set])
               !config      (r/atom cfg-init)
               !scale       (r/cursor !config [:canvas :scale])
               !params      (r/atom params-init)
               !state-log   (r/atom (list {:params params-init
                                           :ui     ui-init}))
               !hov         (r/atom nil)
               !tab         (r/atom :script)
               !shell       (r/atom "")

               report!      (fn [iii i-main shift?]
                              (reset! !sel (-> iii
                                               (with-meta {:main?  (not i-main)
                                                           :shift? shift?}))))
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

               canvas-paint' (with-meta #'render-svg-web/canvas-paint
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
              (update :script els/attach-ii-el-meta*))]

      [:div.canvas
       [:div.sidebar.script-phantom]
       [canvas-sidebar
        !config !params !shell !state-log !tab
        config params params' sel dispatch!]

       [canvas-paint'
        [@!hov sel dispatch! report! report-hov! set-ref! dnd-fns]
        config params']])))
