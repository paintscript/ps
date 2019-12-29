(ns paintscript.app
  (:require [reagent.core :as r]
            [paintscript.canvas :refer [canvas]]))

(defn- root []
  [canvas {:dims [100 100]
           :script [[:line [65 15]]
                    [:curve-C [5 32] [62 35] [64 48]]
                    [:curve-S [18 42] [17 77]]]}])

(defn- mount-root! []
  (r/render [#'root]
            (.getElementById js/document "app")))

(defonce _init (mount-root!))

(defn on-js-reload [] (mount-root!))
