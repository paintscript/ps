(ns paintscript.app
  (:require [reagent.core :as r]
            [paintscript.canvas :refer [canvas]]))

(defn- root []
  [canvas {:dims [100 100]}])

(defn- mount-root! []
  (r/render [#'root]
            (.getElementById js/document "app")))

(defonce _init (mount-root!))

(defn on-js-reload [] (mount-root!))
