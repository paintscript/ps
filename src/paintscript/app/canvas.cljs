(ns paintscript.app.canvas
  (:require [clojure.string :as str]
            [cljs.pprint :refer [pprint]]
            [cljs.reader :refer [read-string]]

            [reagent.core :as r]
            [z-com.core :as zc]
            [keybind.core :as key]

            [paintscript.util :as u]
            [paintscript.els :as els]
            [paintscript.nav :as nav]
            [paintscript.app.ctl :as ctl]
            [paintscript.app.s-log :as s-log]

            [paintscript.render-svg :as render-svg]))

(defn- canvas-sidebar
  [!config !cmpt !ui !shell !s-log !tab
   config cmpt cmpt' sel
   dispatch!]
  (let [tab @!tab
        [src-k-sel pi-sel eli-sel xyi-sel] sel
        status? (and sel (= :script tab))]
    [:div.sidebar.script {:class (when status? "with-status")}
     [:div.controls
      (for [tab-k [:script :config :log]]
        ^{:key tab-k}
        [zc/button
         :label    (name tab-k)
         :active?  (= tab-k tab)
         :on-click #(reset! !tab tab-k)])
      [zc/button
       :label    "+"
       :active?  @(r/cursor !ui [:insert-mode?])
       :on-click #(dispatch! [:toggle-insert])]]

     (case tab
       :script [:textarea
                {:value     (str/trim (u/pprint* cmpt))
                 :on-focus  #(key/disable!)
                 :on-blur   #(key/enable!)
                 :on-change #(reset! !cmpt (-> % .-target .-value read-string))}]
       :config [:textarea
                {:value     (str/trim (u/pprint* config))
                 :on-focus  #(key/disable!)
                 :on-blur   #(key/enable!)
                 :on-change #(reset! !config (-> % .-target .-value read-string))}]
       :log    (let [[i-active i-s-items] (s-log/items !s-log)]
                 [:ol.log
                  (for [[i {:as log-item :keys [n op]}] i-s-items]
                    ^{:key n}
                    [:li.log-item
                     {:class         (when (= i-active i) "active")
                      :on-click      #(s-log/op !s-log !cmpt !ui :activate i)
                      :on-mouse-over #(s-log/op !s-log !cmpt !ui :preview  i)
                      :on-mouse-out  #(s-log/op !s-log !cmpt !ui :activate i-active)}
                     n ". " (pr-str op)])]))

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
      (when status?
        (let [[_ opts :as p] (nav/cmpt> cmpt' :src-k src-k-sel :pi  pi-sel)
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

(defn canvas [cfg-init cmpt0]
  (r/with-let [ui-init      {:sel           nil
                             :sel-set       nil
                             :snap          nil
                             :snap-to-grid? true
                             :insert-mode?  true}

               !ui          (r/atom   ui-init)
               !config      (r/atom   cfg-init)
               !cmpt        (r/atom   cmpt0)
               !s-log       (r/atom   nil)
               !hov         (r/atom   nil)
               !tab         (r/atom   :script)
               !shell       (r/atom   "")

               !sel         (r/cursor !ui     [:sel])
               !sel-set     (r/cursor !ui     [:sel-set])
               !scale       (r/cursor !config [:canvas :scale])

               report!      (fn [iii i-main shift?]
                              (reset! !sel (-> iii
                                               (with-meta {:main?  (not i-main)
                                                           :shift? shift?}))))
               dispatch!    (partial ctl/dispatch! !config !cmpt !s-log !ui)
               dnd-fns      (ctl/drag-and-drop-fns !cmpt !ui dispatch!)
               kb-fns       (ctl/keybind-fns       !cmpt !ui dispatch!)
               report-hov!  (fn [iii val]
                              (swap! !hov #(cond
                                             val iii
                                             (= iii %) nil
                                             :else %)))

               ; (reset! !s-log (s-log/init @!cmpt @!ui))

               _ (doseq [[k f] kb-fns]
                   (key/bind! k (keyword k) f))

               canvas-paint' (with-meta #'render-svg/canvas-paint
                               {:component-did-catch
                                (fn [e info]
                                  (println :paint-error e)
                                  (js/console.log e)
                                  (dispatch! [:undo]))})]

    (let [config  @!config
          cmpt    @!cmpt
          {:keys
           [sel]} @!ui
          cmpt'   (-> (merge-with merge
                                  (-> config (dissoc :script))
                                  cmpt)
                      (update :script els/attach-ii-el-meta*))]

      [:div.canvas
       [:div.sidebar.script-phantom]
       [canvas-sidebar
        !config !cmpt !ui !shell !s-log !tab
        config cmpt cmpt' sel dispatch!]

       [canvas-paint'
        [@!hov sel dispatch! report! report-hov! dnd-fns]
        config cmpt']
       ])))
