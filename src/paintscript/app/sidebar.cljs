
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
            [paintscript.app.sidebar-items :refer [sidebar-items
                                                   cmpt-pth-view]]))

(defn- canvas-sidebar
  [dispatch! !ui !shell !s-log !tab !navr-sel
   conf-ext cmpt-root]
  (let [tab     @!tab
        navr-sel @!navr-sel
        status? (and false ;; TODO: status-stack obsolete?
                     navr-sel
                     (-> tab #{:tab/script
                               :tab/items}))

        ;; NOTE: uses plain cmpt-root (not merged w/ conf-ext)
        [_cmpt-base
         cmpt-sel] (nav/get-cmpt-sel cmpt-root @!navr-sel)]
    [:div.sidebar.script {:class (when status? "with-status")}

     ;; --- controls

     [:div.controls
      (for [tab-k [:tab/items
                   :tab/script
                   :tab/config
                   :tab/log]]
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
       :tab/items  [sidebar-items dispatch! !navr-sel cmpt-root cmpt-sel]

       :tab/script [:div.sidebar-source
                    [:ol.s-els
                     [cmpt-pth-view dispatch! (:cmpt-pth navr-sel)]
                     [:li.textarea
                      [:textarea
                       {:value     (str/trim (u/pprint* cmpt-sel))
                        :on-focus  #(key/disable!)
                        :on-blur   #(key/enable!)
                        :on-change #(dispatch! [:op/set-cmpt-str (-> % .-target .-value)])}]]]]

       :tab/config [:div.sidebar-config
                    [:textarea
                     {:value     (str/trim (u/pprint* conf-ext))
                      :on-focus  #(key/disable!)
                      :on-blur   #(key/enable!)
                      :on-change #(dispatch! [:op/set-config-str (-> % .-target .-value)])}]]

       :tab/log    (let [[i-active i-s-items] (s-log/items !s-log)]
                     [:ol.log
                      (for [[i {:as log-item :keys [n op]}] i-s-items]
                        ^{:key n}
                        [:li.log-item
                         {:class         (when (= i-active i) "active")
                          :on-click      #(dispatch! [:op.s-log/activate i])
                          ; :on-mouse-over #(dispatch! [:op.s-log/preview i])
                          ; :on-mouse-out  #(dispatch! [:op.s-log/activate i-active])
                          }
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
        (let [navr-sel           @!navr-sel
              [_ opts :as s-el] (nav/cmpt> cmpt-sel :src-k (:src-k navr-sel) :s-eli (:x-el-k navr-sel))
              [k & xys]         (nav/s-el> s-el  :p-eli (:p-el-i navr-sel))]
          [:div.selection-stack
           [:div.selection-level.iii
            [:span (-> navr-sel vals pr-str)]
            [:div.controls.crud
             [zc/button :label "blur" :on-click #(dispatch! [:navr-sel nil])]
             [zc/button :label "up" :on-click #(dispatch! [:navr-sel (-> navr-sel
                                                                        nav/nav-up)])]]]

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
            [:span (pr-str (nav/xys> xys :xyi (:xy-i navr-sel)))]
            (when (contains? #{:L :arc} k)
              [:div.controls.crud
               [zc/button :label "add" :on-click #(dispatch! [:xy-append])]
               [zc/button :label "del" :on-click #(dispatch! [:xy-del])]])]]))]]))
