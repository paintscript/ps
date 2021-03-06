(ns paintscript.docs-index
  (:require [reagent.core :as r]
            [xspace.ui.index :as xs-idx]
            [urlkit-util.app-dispatcher :refer [app-dispatcher]]
            [paintscript.util-xspace :refer [util-xspace]]
            [paintscript.path-xspace :refer [path-xspace]]
            [paintscript.ops-xspace :refer [ops-xspace]]
            [paintscript.xspace-view :refer [xspace-paintscript]]))

(defn xspace-index []
  [:div.xspace-index
   [:h2 "xspace-index"]
   [xs-idx/index-seq {:view xspace-paintscript}
    [["paintscript.path" path-xspace]]]])

(defn docs-index []
  [:div.doc-index
   [:h1 "paintscript docs"]
   [app-dispatcher
    (list "xspace")
    {"xspace" {:title "🐒 xspace" :c xs-idx/urlkit-cfg :view [xspace-index]}}]])

(defn mount-root! []
  (r/render [docs-index] (.getElementById js/document "app")))

(defn on-js-reload [] (mount-root!))

(mount-root!)
