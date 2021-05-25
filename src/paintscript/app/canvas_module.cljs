(ns paintscript.app.canvas-module
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

            [paintscript.canvas :as canvas]))

(defn- canvas-sidebar
  [!config !cmpt !ui !shell !s-log !tab
   config cmpt cmpt' sel-rec
   dispatch!]
  (let [tab     @!tab
        status? (and sel-rec
                     (= :script tab))]
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
        (let [[_ opts :as s-el] (nav/cmpt> cmpt' :src-k (:src-k sel-rec) :s-eli (:x-el-k sel-rec))
              [k & xys]         (nav/s-el> s-el  :p-eli (:p-el-i sel-rec))]
          [:div.selection-stack
           [:div.selection-level.iii
            [:span (-> sel-rec nav/pth-rec->vec pr-str)]
            [:div.controls.crud
             [zc/button :label "blur" :on-click #(dispatch! [:sel-rec nil])]
             [zc/button :label "up" :on-click #(dispatch! [:sel-rec (-> sel-rec
                                                                        nav/pth-rec->vec
                                                                        drop-last
                                                                        nav/pth-vec->rec)])]]]

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
            [:span (pr-str (nav/xys> xys :xyi (:xy-i sel-rec)))]
            (when (contains? #{:L :arc} k)
              [:div.controls.crud
               [zc/button :label "add" :on-click #(dispatch! [:xy-append])]
               [zc/button :label "del" :on-click #(dispatch! [:xy-del])]])]]))]]))

(defn canvas [cfg-init cmpt0]
  (r/with-let [ui-init      {:sel-rec       nil
                             :sel-set       nil
                             :snap          nil
                             :snap-to-grid? true
                             :insert-mode?  true}

               !ui          (r/atom   ui-init)
               !config      (r/atom   cfg-init)
               !cmpt        (r/atom   cmpt0)
               !s-log       (r/atom   nil)
               !hov-rec     (r/atom   nil)
               !tab         (r/atom   :script)
               !shell       (r/atom   "")

               !sel-rec     (r/cursor !ui     [:sel-rec])
               !sel-set     (r/cursor !ui     [:sel-set])
               !scale       (r/cursor !config [:canvas :scale])


               dispatch!    (partial ctl/dispatch! !config !cmpt !s-log !ui)
               dnd-fns      (ctl/drag-and-drop-fns !cmpt !ui dispatch!)
               kb-fns       (ctl/keybind-fns       !cmpt !ui dispatch!)

               report-down! (fn [pth-rec i-main shift?]
                              (reset! !sel-rec
                                      (-> pth-rec
                                          (with-meta {:main?  (not i-main)
                                                      :shift? shift?}))))
               report-over! (fn [pth-rec val]
                              (swap! !hov-rec
                                     #(cond
                                        val           pth-rec
                                        (= pth-rec %) nil
                                        :else         %)))

               ; (reset! !s-log (s-log/init @!cmpt @!ui))

               _ (doseq [[k f] kb-fns]
                   (key/bind! k (keyword k) f))

               canvas-paint' (with-meta #'canvas/canvas-paint
                               {:component-did-catch
                                (fn [e info]
                                  (println :paint-error e)
                                  (js/console.log e)
                                  (dispatch! [:undo]))})]

    (let [config  @!config
          cmpt    @!cmpt
          sel-rec @!sel-rec
          hov-rec @!hov-rec
          cmpt'   (-> (merge-with merge
                                  (-> config (dissoc :script))
                                  cmpt)
                      (update :script els/attach-ii-el-meta*))
          c-app   [hov-rec sel-rec dispatch! report-down! report-over! dnd-fns]]

      [:div.canvas
       [:div.sidebar.script-phantom]
       [canvas-sidebar
        !config !cmpt !ui !shell !s-log !tab
        config cmpt cmpt' sel-rec dispatch!]

       [canvas-paint' c-app config cmpt']])))
