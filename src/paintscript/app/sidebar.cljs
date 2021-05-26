
(ns paintscript.app.sidebar
  (:require [clojure.string :as str]
            [clojure.edn :refer [read-string]]

            [svg-hiccup-kit.core :as shk]
            [reagent.core :as r]
            [z-com.core :as zc]
            [keybind.core :as key]

            [paintscript.util :as u]
            [paintscript.nav :as nav]
            [paintscript.app.s-log :as s-log]
            [paintscript.app.sidebar-items :refer [sidebar-items]]))

(defn- canvas-sidebar
  [!config !cmpt !ui !shell !s-log !tab
   config cmpt cmpt' sel-rec
   dispatch!]
  (let [tab     @!tab
        status? (and sel-rec
                     (-> tab #{:tab/script
                               :tab/items}))]
    [:div.sidebar.script {:class (when status? "with-status")}

     ;; --- controls

     [:div.controls
      (for [tab-k [:tab/items :tab/script :tab/config :tab/log]]
        ^{:key tab-k}
        [zc/button
         :label    (name tab-k)
         :active?  (= tab-k tab)
         :on-click #(reset! !tab tab-k)])
      [zc/button
       :label    "+"
       :active?  @(r/cursor !ui [:insert-mode?])
       :on-click #(dispatch! [:toggle-insert])]]

     ;; --- main

     (case tab
       :tab/items  [sidebar-items dispatch! cmpt sel-rec]

       :tab/script [:textarea
                    {:value     (str/trim (u/pprint* cmpt))
                     :on-focus  #(key/disable!)
                     :on-blur   #(key/enable!)
                     :on-change #(reset! !cmpt (-> % .-target .-value read-string))}]

       :tab/config [:textarea
                    {:value     (str/trim (u/pprint* config))
                     :on-focus  #(key/disable!)
                     :on-blur   #(key/enable!)
                     :on-change #(reset! !config (-> % .-target .-value read-string))}]

       :tab/log    (let [[i-active i-s-items] (s-log/items !s-log)]
                     [:ol.log
                      (for [[i {:as log-item :keys [n op]}] i-s-items]
                        ^{:key n}
                        [:li.log-item
                         {:class         (when (= i-active i) "active")
                          :on-click      #(s-log/op !s-log !cmpt !ui :activate i)
                          :on-mouse-over #(s-log/op !s-log !cmpt !ui :preview  i)
                          :on-mouse-out  #(s-log/op !s-log !cmpt !ui :activate i-active)}
                         n ". " (pr-str op)])]))

     ;; --- shell

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

     ;; --- status stack

     [:div.status
      (when status?
        (let [
              cmpt-sub          (-> cmpt'
                                    (cond-> (:cmpt-pth sel-rec)
                                            (nav/get-cmpt-sel sel-rec)))

              [_ opts :as s-el] (nav/cmpt> cmpt-sub :src-k (:src-k sel-rec) :s-eli (:x-el-k sel-rec))
              [k & xys]         (nav/s-el> s-el  :p-eli (:p-el-i sel-rec))]
          [:div.selection-stack
           [:div.selection-level.iii
            [:span (-> sel-rec vals pr-str)]
            [:div.controls.crud
             [zc/button :label "blur" :on-click #(dispatch! [:sel-rec nil])]
             [zc/button :label "up" :on-click #(dispatch! [:sel-rec (-> sel-rec
                                                                        nav/pth-up)])]]]

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
